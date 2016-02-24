{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString,
  drawingFromDecl,
  drawingsFromModule
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude((<>))

import Language.Haskell.Exts(Decl(..), parseDecl, Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), fromParseResult, Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..), Alt(..), Module(..))
import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(State, evalState)
import Debug.Trace
import Data.Either(partitionEithers, rights)
import Data.List(unzip4, partition)
import Control.Monad(replicateM)

import Types(Icon, Edge(..), Drawing(..), NameAndPort(..), IDState,
  initialIdState, getId)
import Util(toNames, noEnds, nameAndPort, justName, mapFst)
import Icons(Icon(..))

type Reference = Either String NameAndPort
-- | An IconGraph is a normal Drawing (Icons, Edges, and sub Drawings) with two additional fields:
-- unconected sink ports (varible usage), and unconnected source ports (varible definition).
data IconGraph = IconGraph {
  igIcons :: [(DIA.Name, Icon)],
  igEdges :: [Edge],
  igSubDrawings :: [(DIA.Name, Drawing)],
  igSinks :: [(String, NameAndPort)],
  igBindings :: [(String, Reference)]}
  deriving (Show)

type EvalContext = [String]
type GraphAndRef = (IconGraph, Reference)
type Sink = (String, NameAndPort)

instance DIA.Semigroup IconGraph where
  (IconGraph icons1 edges1 subDrawings1 sinks1 sources1) <> (IconGraph icons2 edges2 subDrawings2 sinks2 sources2) =
    IconGraph (icons1 <> icons2) (edges1 <> edges2) (subDrawings1 <> subDrawings2) (sinks1 <> sinks2) (sources1 <> sources2)

instance Monoid IconGraph where
  mempty = IconGraph mempty mempty mempty mempty mempty
  mappend = (<>)

iconGraphFromIcons :: [(DIA.Name, Icon)] -> IconGraph
iconGraphFromIcons icons = IconGraph icons mempty mempty mempty mempty

iconGraphFromIconsEdges :: [(DIA.Name, Icon)] -> [Edge] -> IconGraph
iconGraphFromIconsEdges icons edges = IconGraph icons edges mempty mempty mempty

getUniqueName :: String -> State IDState String
getUniqueName base = fmap ((base ++). show) getId

nameToString :: Language.Haskell.Exts.Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

evalPApp :: QName -> [Pat] -> State IDState (IconGraph, NameAndPort)
evalPApp name patterns = do
  patName <- DIA.toName <$> getUniqueName "pat"
  let
    context = mempty
  evaledPatterns <- mapM evalPattern patterns
  let
    constructorName = evalQName name context
    gr = makeApplyGraph True patName constructorName evaledPatterns (length evaledPatterns)
  pure gr


evalPLit :: Exts.Sign -> Exts.Literal -> State IDState (IconGraph, NameAndPort)
evalPLit Exts.Signless l = evalLit l
evalPLit Exts.Negative l = makeBox ('-' : showLiteral l)

evalPattern :: Pat -> State IDState GraphAndRef
evalPattern p = case p of
  PVar n -> pure (mempty, Left $ nameToString n)
  PLit s l -> fmap Right <$> evalPLit s l
  PApp name patterns -> fmap Right <$> evalPApp name patterns
  -- TODO special tuple handling.
  PTuple box patterns -> fmap Right <$> evalPApp (Exts.UnQual $ Ident "(,)") patterns
  PParen pat -> evalPattern pat
  PWildCard -> fmap Right <$> makeBox "_"

evalQName :: QName -> EvalContext -> (IconGraph, Reference)
evalQName (UnQual n) context = result where
  nameString = nameToString n
  graph = iconGraphFromIcons [(DIA.toName nameString, TextBoxIcon nameString)]
  result = if nameString `elem` context
    then (mempty, Left nameString)
    else (graph, Right $ justName nameString)
-- TODO remove initialIdState
evalQName (Special Exts.UnitCon) _ = Right <$> evalState (makeBox "()") initialIdState

evalQOp :: QOp -> EvalContext -> (IconGraph, Reference)
evalQOp (QVarOp n) = evalQName n
evalQOp (QConOp n) = evalQName n

-- TODO: Refactor with combineExpressions
edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> IconGraph
edgesForRefPortList inPattern portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  mkGraph (ref, port) = case ref of
    Left str -> if inPattern
      then IconGraph mempty mempty mempty mempty [(str, Right port)]
      else IconGraph mempty mempty mempty [(str, port)] mempty
    Right resultPort -> IconGraph mempty [Edge (resultPort, port) noEnds] mempty mempty mempty

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> IconGraph
combineExpressions inPattern portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  mkGraph ((graph, ref), port) = graph <> case ref of
    Left str -> if inPattern
      then IconGraph mempty mempty mempty mempty [(str, Right port)]
      else IconGraph mempty mempty mempty [(str, port)] mempty
    Right resultPort -> IconGraph mempty [Edge (resultPort, port) noEnds] mempty mempty mempty

makeApplyGraph :: Bool -> DIA.Name -> (IconGraph, Reference) -> [(IconGraph, Reference)] -> Int -> (IconGraph, NameAndPort)
makeApplyGraph inPattern applyIconName funVal argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions inPattern $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, Apply0NIcon numArgs)]
    newGraph = iconGraphFromIcons icons

evalApp :: (Exp, [Exp]) -> EvalContext -> State IDState (IconGraph, NameAndPort)
evalApp (funExp, argExps) c = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  pure $ makeApplyGraph False applyIconName funVal argVals (length argExps)

evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState (IconGraph, NameAndPort)
evalInfixApp c e1 op e2 = do
  argVals <- mapM (evalExp c) [e1, e2]
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  let funVal = evalQOp op c
  pure $ makeApplyGraph False applyIconName funVal argVals 2

-- TODO add test for this function
simplifyApp :: Exp -> (Exp, [Exp])
simplifyApp (App exp1 exp2) = (funExp, args <> [exp2])
  where
    (funExp, args) = simplifyApp exp1
simplifyApp e = (e, [])

evalIf :: EvalContext -> Exp -> Exp -> Exp -> State IDState (IconGraph, NameAndPort)
evalIf c e1 e2 e3 = do
  e1Val <- evalExp c e1
  e2Val <- evalExp c e2
  e3Val <- evalExp c e3
  guardName <- DIA.toName <$> getUniqueName "if"
  let
    icons = [(guardName, GuardIcon 2)]
    combinedGraph =
      combineExpressions False $ zip [e1Val, e2Val, e3Val] (map (nameAndPort guardName) [3, 2, 4])
    newGraph = iconGraphFromIcons icons <> combinedGraph
  pure (newGraph, NameAndPort guardName (Just 0))

evalStmt :: EvalContext -> Stmt -> State IDState GraphAndRef
evalStmt c (Qualifier e) = evalExp c e

evalStmts :: EvalContext -> [Stmt] -> State IDState GraphAndRef
evalStmts c [stmt] = evalStmt c stmt

evalGuaredRhs :: EvalContext -> GuardedRhs -> State IDState (GraphAndRef, GraphAndRef)
evalGuaredRhs c (GuardedRhs _ stmts e) = do
  expVal <- evalExp c e
  stmtsVal <- evalStmts c stmts
  pure (stmtsVal, expVal)

evalGuardedRhss :: EvalContext -> [GuardedRhs] -> State IDState (IconGraph, NameAndPort)
evalGuardedRhss c rhss = do
  guardName <- DIA.toName <$> getUniqueName "guard"
  evaledRhss <- mapM (evalGuaredRhs c) rhss
  let
    (bools, exps) = unzip evaledRhss
    expsWithPorts = zip exps $ map (nameAndPort guardName) [2,4..]
    boolsWithPorts = zip bools $ map (nameAndPort guardName) [3,5..]
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [(guardName, GuardIcon (length rhss))]
    newGraph = iconGraphFromIcons icons <> combindedGraph
  pure (newGraph, NameAndPort guardName (Just 0))

makeBox :: String -> State IDState (IconGraph, NameAndPort)
makeBox str = do
  name <- DIA.toName <$> getUniqueName str
  let graph = iconGraphFromIcons [(DIA.toName name, TextBoxIcon str)]
  pure (graph, justName name)

makeLiteral :: (Show x) => x -> State IDState (IconGraph, NameAndPort)
makeLiteral = makeBox. show

evalLit :: Exts.Literal -> State IDState (IconGraph, NameAndPort)
evalLit (Exts.Int x) = makeLiteral x
evalLit (Exts.Char x) = makeLiteral x
evalLit (Exts.String x) = makeLiteral x
-- TODO: Print the Rational as a floating point.
evalLit (Exts.Frac x) = makeLiteral x
-- TODO: Test the unboxed literals
evalLit (Exts.PrimInt x) = makeLiteral x
evalLit (Exts.PrimWord x) = makeLiteral x
evalLit (Exts.PrimFloat x) = makeLiteral x
evalLit (Exts.PrimDouble x) = makeLiteral x
evalLit (Exts.PrimChar x) = makeLiteral x
evalLit (Exts.PrimString x) = makeLiteral x

showLiteral :: Exts.Literal -> String
showLiteral (Exts.Int x) = show x
showLiteral (Exts.Char x) = show x
showLiteral (Exts.String x) = show x
-- TODO: Print the Rational as a floating point.
showLiteral (Exts.Frac x) = show x
-- TODO: Test the unboxed literals
showLiteral (Exts.PrimInt x) = show x
showLiteral (Exts.PrimWord x) = show x
showLiteral (Exts.PrimFloat x) = show x
showLiteral (Exts.PrimDouble x) = show x
showLiteral (Exts.PrimChar x) = show x
showLiteral (Exts.PrimString x) = show x


namesInPattern :: GraphAndRef -> [String]
namesInPattern (_, Left str) = [str]
namesInPattern (IconGraph _ _ _ _ bindings, Right _) = fmap fst bindings

getBoundVarName :: Decl -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _) = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind [Match _ name _ _ _ _]) = [nameToString name]
getBoundVarName (FunBind (Match _ name _ _ _ _:_)) = [nameToString name]

--TODO: Should this call makeEdges?
evalBinds :: EvalContext -> Binds -> State IDState (IconGraph, EvalContext)
evalBinds c (BDecls decls) = do
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  evaledDecl <- mconcat <$> mapM (evalDecl augmentedContext) decls
  pure (evaledDecl, augmentedContext)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

-- | Recursivly find the matching reference in a list of bindings.
-- TODO: Might want to present some indication if there is a reference cycle.
lookupReference :: [(String, Reference)] -> Reference -> Reference
lookupReference _ ref@(Right _) = ref
lookupReference bindings ref@(Left originalS) = lookupHelper ref where
  lookupHelper newRef@(Right _) = newRef
  lookupHelper newRef@(Left s)= case lookup s bindings of
    Just r -> failIfCycle r $ lookupHelper r
    Nothing -> newRef
    where
      failIfCycle r@(Left newStr) res = if newStr == originalS then r else res
      failIfCycle _ res = res

deleteBindings :: IconGraph -> IconGraph
deleteBindings (IconGraph a b c d _) = IconGraph a b c d mempty

makeEdgesCore :: [Sink] -> [(String, Reference)] -> ([Sink], [Edge])
makeEdgesCore sinks bindings = partitionEithers $ fmap renameOrMakeEdge sinks
  where
    renameOrMakeEdge :: (String, NameAndPort) -> Either (String, NameAndPort) Edge
    renameOrMakeEdge orig@(s, destPort) = case lookup s bindings of
      Just ref -> case lookupReference bindings ref of
        (Right sourcePort) -> Right $ Edge (sourcePort, destPort) noEnds
        (Left newStr) -> Left (newStr, destPort)
      Nothing -> Left orig

makeEdges :: IconGraph -> IconGraph
makeEdges (IconGraph icons edges c sinks bindings) = newGraph where
  (newSinks, newEdges) = makeEdgesCore sinks bindings
  newGraph = IconGraph icons (newEdges <> edges) c newSinks bindings

evalGeneralLet :: (EvalContext -> State IDState (IconGraph, Reference)) -> EvalContext -> Binds -> State IDState (IconGraph, Reference)
evalGeneralLet expOrRhsEvaler c bs = do
  (bindGraph, bindContext) <- evalBinds c bs
  expVal <- expOrRhsEvaler bindContext
  let
    (expGraph, expResult) = expVal
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
    (IconGraph _ _ _ _ bindings) = bindGraph
  pure (newGraph, lookupReference bindings expResult)

evalLet :: EvalContext -> Binds -> Exp -> State IDState (IconGraph, Reference)
evalLet context binds e = evalGeneralLet (`evalExp` e) context binds

-- TODO: Refactor this with evalPatBind
evalPatAndRhs :: EvalContext -> Pat -> Rhs -> Maybe Binds -> State IDState (Bool, IconGraph, Reference, NameAndPort)
evalPatAndRhs c pat rhs maybeWhereBinds = do
  patternNames <- namesInPattern <$> evalPattern pat
  let rhsContext = patternNames <> c
  -- TODO: remove coerceExpressionResult
  (rhsGraph, rhsRef) <- rhsWithBinds maybeWhereBinds rhs rhsContext >>= coerceExpressionResult
  (patGraph, patRef) <- evalPattern pat
  caseIconName <- DIA.toName <$> getUniqueName "case"
  let
    grWithEdges = makeEdges (rhsGraph <> patGraph)
    -- The pattern and rhs are conneted if makeEdges added extra edges.
    patRhsAreConnected =
      length (igEdges grWithEdges) > (length (igEdges rhsGraph) + length (igEdges patGraph))
  pure (patRhsAreConnected, deleteBindings grWithEdges, patRef, rhsRef)

-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: EvalContext -> Exts.Alt -> State IDState (Bool, IconGraph, Reference, NameAndPort)
evalAlt c (Exts.Alt s pat rhs maybeBinds) = evalPatAndRhs c pat rhs maybeBinds

evalCase :: EvalContext -> Exp -> [Alt] -> State IDState (IconGraph, NameAndPort)
evalCase c e alts = do
  evaledAlts <- mapM (evalAlt c) alts
  (expGraph, expRef) <- evalExp c e
  caseIconName <- getUniqueName "case"
  let
    (patRhsConnected, altGraphs, patRefs, rhsRefs) = unzip4 evaledAlts
    combindedAltGraph = mconcat altGraphs
    numAlts = length alts
    icons = toNames [(caseIconName, CaseIcon numAlts)]
    caseGraph = iconGraphFromIcons icons
    expEdge = (expRef, nameAndPort caseIconName 0)
    patEdges = zip patRefs $ map (nameAndPort caseIconName ) [2,4..]
    rhsEdges = zip patRhsConnected $ zip rhsRefs $ map (nameAndPort caseIconName) [3,5..]
    (connectedRhss, unConnectedRhss) = partition fst rhsEdges
  resultIconNames <- replicateM numAlts (getUniqueName "caseResult")
  let
    makeCaseResult resultIconName rhsPort = iconGraphFromIconsEdges rhsNewIcons rhsNewEdges
      where
        rhsNewIcons = toNames [(resultIconName, CaseResultIcon)]
        rhsNewEdges = [Edge (rhsPort, justName resultIconName) noEnds]
    caseResultGraphs = mconcat $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
    filteredRhsEdges = mapFst Right $ fmap snd unConnectedRhss
    caseEdgeGraph = edgesForRefPortList False $ expEdge : (patEdges <> filteredRhsEdges)
    finalGraph = caseResultGraphs <> expGraph <> caseEdgeGraph <> caseGraph <> combindedAltGraph
  pure (finalGraph, nameAndPort caseIconName 1)

evalTuple :: EvalContext -> [Exp] -> State IDState (IconGraph, NameAndPort)
evalTuple c exps = do
  argVals <- mapM (evalExp c) exps
  funVal <- makeBox "(,)"
  applyIconName <- DIA.toName <$> getUniqueName "tupleApp"
  pure $ makeApplyGraph False applyIconName (fmap Right funVal) argVals (length exps)

evalExp :: EvalContext  -> Exp -> State IDState (IconGraph, Reference)
evalExp c x = case x of
  Var n -> pure $ evalQName n c
  Con n -> pure $ evalQName n c
  Lit l -> fmap Right <$> evalLit l
  InfixApp e1 op e2 -> fmap Right <$> evalInfixApp c e1 op e2
  e@App{} -> fmap Right <$> evalApp (simplifyApp e) c
  Lambda _ patterns e -> fmap Right <$> evalLambda c patterns e
  Let bs e -> evalLet c bs e
  If e1 e2 e3 -> fmap Right <$> evalIf c e1 e2 e3
  Case e alts -> fmap Right <$> evalCase c e alts
  -- TODO special tuple symbol
  Tuple _ exps -> fmap Right <$> evalTuple c exps
  Paren e -> evalExp c e

-- | This is used by the rhs for identity (eg. y x = x)
makeDummyRhs :: String -> State IDState (IconGraph, NameAndPort)
makeDummyRhs s = do
  iconName <- getUniqueName s
  let
    graph = IconGraph icons mempty mempty [(s, port)] mempty
    icons = [(DIA.toName iconName, BranchIcon)]
    port = justName iconName
  pure (graph, port)

coerceExpressionResult :: (IconGraph, Reference) -> State IDState (IconGraph, NameAndPort)
coerceExpressionResult (_, Left str) = makeDummyRhs str
coerceExpressionResult (g, Right x) = pure (g, x)

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: EvalContext -> Rhs -> State IDState (IconGraph, Reference)
evalRhs c (UnGuardedRhs e) = evalExp c e
evalRhs c (GuardedRhss rhss) = fmap Right <$> evalGuardedRhss c rhss

rhsWithBinds :: Maybe Binds -> Rhs -> EvalContext -> State IDState (IconGraph, Reference)
rhsWithBinds maybeWhereBinds rhs rhsContext = case maybeWhereBinds of
  Nothing -> evalRhs rhsContext rhs
  Just b -> evalGeneralLet (`evalRhs` rhs) rhsContext b

evalPatBind :: EvalContext -> Decl -> State IDState IconGraph
evalPatBind c (PatBind _ pat rhs maybeWhereBinds) = do
  patternNames <- namesInPattern <$> evalPattern pat
  let rhsContext = patternNames <> c
  (rhsGraph, rhsRef) <- rhsWithBinds maybeWhereBinds rhs rhsContext
  (patGraph, patRef) <- evalPattern pat
  let
    (newEdges, newSinks, bindings) = case patRef of
      (Left s) -> (mempty, mempty, [(s, rhsRef)])
      (Right patPort) -> case rhsRef of
        (Left rhsStr) -> (mempty, [(rhsStr, patPort)], mempty)
        -- TODO: This edge should be special to indicate that one side is a pattern.
        (Right rhsPort) -> ([Edge (rhsPort, patPort) noEnds], mempty, mempty)
    gr = IconGraph mempty newEdges mempty newSinks bindings
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _ _) = Drawing icons edges subDrawings

makeRhsDrawing :: DIA.IsName a => a -> (IconGraph, NameAndPort) -> Drawing
makeRhsDrawing resultIconName (rhsGraph, rhsResult)= rhsDrawing where
  rhsNewIcons = toNames [(resultIconName, ResultIcon)]
  rhsNewEdges = [Edge (rhsResult, justName resultIconName) noEnds]
  rhsGraphWithResult = rhsGraph <> iconGraphFromIconsEdges rhsNewIcons rhsNewEdges
  rhsDrawing = iconGraphToDrawing rhsGraphWithResult

qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

makePatternEdges :: String -> GraphAndRef -> NameAndPort -> Either IconGraph (String, Reference)
makePatternEdges lambdaName (_, Right patPort) lamPort =
  Left $ iconGraphFromIconsEdges mempty
    [Edge (lamPort, qualifyNameAndPort lambdaName patPort) noEnds]
makePatternEdges _ (_, Left str) lamPort = Right (str, Right lamPort)

generalEvalLambda :: EvalContext -> [Pat] -> (EvalContext -> State IDState GraphAndRef) -> State IDState (IconGraph, NameAndPort)
generalEvalLambda context patterns rhsEvalFun = do
  lambdaName <- getUniqueName "lam"
  patternVals <- mapM evalPattern patterns
  let
    patternStrings = concatMap namesInPattern patternVals
    rhsContext = patternStrings <> context
    lambdaPorts = map (nameAndPort lambdaName) [0,1..]
    patternGraph = mconcat $ map fst patternVals

    (patternEdgeGraphs, rawNewBinds) =
      partitionEithers $ zipWith (makePatternEdges lambdaName) patternVals lambdaPorts
    patternEdgeGraph = mconcat patternEdgeGraphs

    newBinds = rawNewBinds
    numParameters = length patterns
  -- TODO remove coerceExpressionResult here
  (rhsRawGraph, rhsResult) <- rhsEvalFun rhsContext >>= coerceExpressionResult
  resultIconName <- getUniqueName "res"
  rhsDrawingName <- DIA.toName <$> getUniqueName "rhsDraw"
  let
    rhsAndPatternGraph@(IconGraph _ _ _ sinks _) = makeEdges $ patternGraph <> rhsRawGraph
    qualifiedSinks = fmap (fmap (qualifyNameAndPort lambdaName)) sinks
    (newSinks, internalEdges) = makeEdgesCore qualifiedSinks newBinds
    rhsDrawing = makeRhsDrawing resultIconName (rhsAndPatternGraph, rhsResult)
    icons = toNames [(lambdaName, LambdaRegionIcon numParameters rhsDrawingName)]
    finalGraph = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)]
      newSinks mempty
  pure (patternEdgeGraph <> finalGraph, justName lambdaName)

evalLambda :: EvalContext -> [Pat] -> Exp -> State IDState (IconGraph, NameAndPort)
evalLambda c patterns e = generalEvalLambda c patterns (`evalExp` e)

evalMatch :: EvalContext -> Match -> State IDState IconGraph
evalMatch c (Match _ name patterns _ rhs maybeWhereBinds) = do
  let
    matchFunNameString = nameToString name
    newContext = matchFunNameString : c
  (lambdaGraph, lambdaPort) <-
    generalEvalLambda newContext patterns (rhsWithBinds maybeWhereBinds rhs)
  let
    newBinding = IconGraph mempty mempty mempty mempty [(matchFunNameString, Right lambdaPort)]
  pure $ makeEdges (newBinding <> lambdaGraph)

-- TODO If only one pattern don't tuple and untuple.
-- Warning: [] not matched.
matchesToCase :: [Match] -> State IDState Match
matchesToCase [match] = pure match
matchesToCase matches@(Match srcLoc funName pats mType _ _:_) = do
  tempStrings <- replicateM (length pats) (getUniqueName "_tempvar")
  let
    tempPats = fmap (PVar . Ident) tempStrings
    tempVars = fmap (Var . UnQual . Ident) tempStrings
    tuple = Tuple Exts.Boxed tempVars
    alts = fmap matchToAlt matches
    caseExp = Case tuple alts
    rhs = UnGuardedRhs caseExp
    match = Match srcLoc funName tempPats mType rhs Nothing

    matchToAlt :: Match -> Alt
    matchToAlt (Match srcLoc _ pats _ rhs binds) = Alt srcLoc tuplePat rhs binds where
      tuplePat = PTuple Exts.Boxed pats
  pure match

evalMatches :: EvalContext -> [Match] -> State IDState IconGraph
evalMatches _ [] = pure mempty
evalMatches c matches = matchesToCase matches >>= evalMatch c
  --mconcat <$> mapM (evalMatch c) matches
-- TODO turn more than one match into a case expression.

-- TODO: Use the context in evalPatBind and evalMatches
evalDecl :: EvalContext -> Decl -> State IDState IconGraph
evalDecl c d = evaluatedDecl where
  evaluatedDecl = case d of
    pat@PatBind{} -> evalPatBind c pat
    FunBind matches -> evalMatches c matches
    --TODO: Add other cases here
    _ -> pure mempty

showTopLevelBinds :: IconGraph -> State IDState IconGraph
showTopLevelBinds gr@(IconGraph _ _ _ _ binds) = do
  let
    addBind (_, Left _) = pure mempty
    addBind (patName, Right port) = do
      uniquePatName <- getUniqueName patName
      let
        icons = toNames [(uniquePatName, TextBoxIcon patName)]
        edges = [Edge (justName uniquePatName, port) noEnds]
        edgeGraph = iconGraphFromIconsEdges icons edges
      pure edgeGraph
  newGraph <- mconcat <$> mapM addBind binds
  pure $ newGraph <> gr

drawingFromDecl :: Decl -> Drawing
drawingFromDecl d = iconGraphToDrawing $ evalState evaluatedDecl initialIdState
  where evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds

-- Profiling: about 1.5% of time.
translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  drawing = drawingFromDecl decl

drawingsFromModule :: Module -> [Drawing]
drawingsFromModule (Module _ _ _ _ _ _ decls) = fmap drawingFromDecl decls
