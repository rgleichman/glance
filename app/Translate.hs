{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude((<>))

import Language.Haskell.Exts(Decl(..), parseDecl, Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), fromParseResult, Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..))
import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(State, evalState)
import Data.List(partition)
import qualified Control.Arrow
import Debug.Trace
import Data.Either(partitionEithers)

import Types(Icon, Edge(..), Drawing(..), NameAndPort(..), IDState,
  initialIdState, getId)
import Util(toNames, noEnds, nameAndPort, justName, fromMaybeError)
import Icons(Icon(..))

type Reference = Either String NameAndPort
-- | An IconGraph is a normal Drawing (Icons, Edges, and sub Drawings) with two additional fields:
-- unconected sink ports (varible usage), and unconnected source ports (varible definition).
data IconGraph = IconGraph [(DIA.Name, Icon)] [Edge] [(DIA.Name, Drawing)] [(String, NameAndPort)] [(String, Reference)]
  deriving (Show)

type EvalContext = [String]
type GraphAndRef = (IconGraph, Reference)

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


evalPattern :: Pat -> State IDState GraphAndRef
evalPattern p = case p of
  PVar n -> pure (mempty, Left $ nameToString n)
  PApp name patterns -> fmap Right <$> evalPApp name patterns
  PParen pat -> evalPattern pat

evalQName :: QName -> EvalContext -> (IconGraph, Reference)
evalQName (UnQual n) context = result where
  nameString = nameToString n
  graph = iconGraphFromIcons [(DIA.toName nameString, TextBoxIcon nameString)]
  result = if nameString `elem` context
    then (mempty, Left nameString)
    else (graph, Right $ justName nameString)

evalQOp :: QOp -> EvalContext -> (IconGraph, Reference)
evalQOp (QVarOp n) = evalQName n
evalQOp (QConOp n) = evalQName n

combineExpressions :: Bool -> [((IconGraph, Reference), NameAndPort)] -> IconGraph
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

makeLiteral :: (Show x) => x -> State IDState (IconGraph, NameAndPort)
makeLiteral x = do
  let str = show x
  name <- DIA.toName <$> getUniqueName str
  let graph = iconGraphFromIcons [(DIA.toName name, TextBoxIcon str)]
  pure (graph, justName name)

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

namesInPattern :: GraphAndRef -> [String]
namesInPattern (_, Left str) = [str]
namesInPattern (IconGraph _ _ _ _ bindings, Right _) = fmap fst bindings

getBoundVarName :: Decl -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _) = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind [Match _ name _ _ _ _]) = [nameToString name]

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

makeEdges :: IconGraph -> IconGraph
makeEdges (IconGraph icons edges c sinks bindings) = newGraph where
  (newSinks, newEdges) = partitionEithers $ fmap renameOrMakeEdge sinks
  newGraph = IconGraph icons (newEdges <> edges) c newSinks bindings

  renameOrMakeEdge :: (String, NameAndPort) -> Either (String, NameAndPort) Edge
  renameOrMakeEdge orig@(s, destPort) = case lookup s bindings of
    Just ref -> case lookupReference bindings ref of
      (Right sourcePort) -> Right $ Edge (sourcePort, destPort) noEnds
      (Left newStr) -> Left (newStr, destPort)
    Nothing -> Left orig

evalGeneralLet :: (EvalContext -> State IDState (IconGraph, Reference)) -> EvalContext -> Binds -> State IDState (IconGraph, Reference)
evalGeneralLet expOrRhsEvaler c bs = do
  (bindGraph, bindContext) <- evalBinds c bs
  expVal <- expOrRhsEvaler bindContext
  let
    (expGraph, expResult) = expVal
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
    (IconGraph _ _ _ _ bindings) = bindGraph
  pure $ printSelf (newGraph, lookupReference bindings expResult)

evalLet :: EvalContext -> Binds -> Exp -> State IDState (IconGraph, Reference)
evalLet context binds e = evalGeneralLet (`evalExp` e) context binds

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
  Paren e -> evalExp c e

-- | This is used by the rhs for identity (eg. y x = x)
makeDummyRhs :: String -> (IconGraph, NameAndPort)
makeDummyRhs s = (graph, port) where
  graph = IconGraph icons mempty mempty [(s, justName s)] mempty
  icons = [(DIA.toName s, BranchIcon)]
  port = justName s

coerceExpressionResult :: (IconGraph, Reference) -> (IconGraph, NameAndPort)
coerceExpressionResult (_, Left str) = makeDummyRhs str
coerceExpressionResult (g, Right x) = (g, x)

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: Rhs -> EvalContext -> State IDState (IconGraph, Reference)
evalRhs (UnGuardedRhs e) c = evalExp c e
evalRhs (GuardedRhss rhss) c = fmap Right <$> evalGuardedRhss c rhss

evalPatBind :: EvalContext -> Decl -> State IDState IconGraph
evalPatBind c (PatBind _ pat rhs maybeWhereBinds) = do
  patternNames <- printSelf . namesInPattern <$> evalPattern pat
  let
    rhsContext = patternNames <> c
  (rhsGraph, rhsRef) <- case maybeWhereBinds of
    Nothing -> evalRhs rhs rhsContext
    Just b -> evalGeneralLet (evalRhs rhs) rhsContext b

  (patGraph, patRef) <- evalPattern pat
  let
    (newEdges, newSinks, bindings) = case patRef of
      (Left s) -> (mempty, mempty, [(s, rhsRef)])
      (Right patPort) -> case rhsRef of
        (Left rhsStr) -> (mempty, [(rhsStr, patPort)], mempty)
        -- TODO: This edge should be special to indicate that one side is a pattern.
        (Right rhsPort) -> ([Edge (rhsPort, patPort) noEnds], mempty, mempty)
    gr = IconGraph mempty newEdges mempty newSinks bindings
  pure .printSelf. makeEdges $ (gr <> rhsGraph <> patGraph)

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _ _) = Drawing icons edges subDrawings

--processPatterns :: DIA.IsName a => a -> [Pat] -> ([(String, NameAndPort)], [String], Int)
processPatterns :: DIA.IsName a  => a -> [Pat] -> [(String, NameAndPort)] -> ([(String, NameAndPort)], [String], Int)
processPatterns lambdaName patterns extraVars =
  (patternStringMap, patternStrings, numParameters)
    where
      lambdaPorts = map (nameAndPort lambdaName) [0,1..]
      -- TODO this is wrong and must be rewritten for more complex patterns. (perhaps use makeEdges)
      patternStringMap = extraVars <> zip (map (head . namesInPattern. (`evalState` initialIdState) .evalPattern) patterns) lambdaPorts
      patternStrings = map fst patternStringMap
      numParameters = length patterns

makeRhsDrawing :: DIA.IsName a => a -> (IconGraph, NameAndPort) -> Drawing
makeRhsDrawing resultIconName (rhsGraph, rhsResult)= rhsDrawing where
  rhsNewIcons = toNames [(resultIconName, ResultIcon)]
  rhsNewEdges = [Edge (rhsResult, justName resultIconName) noEnds]
  rhsGraphWithResult = rhsGraph <> iconGraphFromIconsEdges rhsNewIcons rhsNewEdges
  rhsDrawing = iconGraphToDrawing rhsGraphWithResult

qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

boundVarsToEdge :: Eq a => [(a, NameAndPort)] -> (a, NameAndPort) -> Edge
boundVarsToEdge patternStringMap (s, np) = Edge (source, np) noEnds where
  source = fromMaybeError "boundVarsToEdge: bound var not found" $ lookup s patternStringMap

--TODO: I think this will loop on recursive references (eg. ("a", Left "a"))
-- simplifyReferences :: [(String, Reference)] -> [(String, Reference)] -> [(String, NameAndPort)]
-- simplifyReferences extraBounds ls = map lookupReference ls where
--   augmentedLs = extraBounds <> ls
--   lookupReference (str, Right n@(NameAndPort _ _)) = (str, n)
--   lookupReference v@(str, Left n) = case lookup n augmentedLs of
--     Just x -> lookupReference (str, x)
--     Nothing -> error $ "Could not find reference. ls =" ++ show ls ++ "\nv=" ++ show v

makeInternalEdges :: Foldable t => String -> IconGraph -> t String -> [(String, NameAndPort)] -> ([Edge], [(String, NameAndPort)])
makeInternalEdges lambdaName rhsGraph patternStrings patternStringMap = (internalEdges, unmatchedBoundVars) where
  (IconGraph _ _ _ boundVars _) = rhsGraph
  qualifiedBoundVars =
    fmap (Control.Arrow.second (qualifyNameAndPort lambdaName)) boundVars
  (matchedBoundVars, unmatchedBoundVars) = partition (\(s, _) -> s `elem` patternStrings) qualifiedBoundVars
  internalEdges = fmap (boundVarsToEdge patternStringMap) matchedBoundVars

evalLambda :: EvalContext -> [Pat] -> Exp -> State IDState (IconGraph, NameAndPort)
evalLambda c patterns e = do
  lambdaName <- getUniqueName "lam"
  let
    (patternStringMap, patternStrings, numParameters) = processPatterns lambdaName patterns []
    augmentedContext = patternStrings <> c
  rhsVal <- evalExp augmentedContext e
  resultIconName <- getUniqueName "res"
  rhsDrawingName <- DIA.toName <$> getUniqueName "rhsDraw"
  let
    -- TODO remove coerceExpressionResult here
    rhsCoercedVal@(rhsGraph, _) = coerceExpressionResult rhsVal
    rhsDrawing = makeRhsDrawing resultIconName rhsCoercedVal
    icons = toNames [(lambdaName, LambdaRegionIcon numParameters rhsDrawingName)]
    (internalEdges, unmatchedBoundVars) =
      makeInternalEdges lambdaName rhsGraph patternStrings patternStringMap
    drawing = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)] unmatchedBoundVars mempty
  pure (drawing, justName lambdaName)

makePatternEdges :: String -> GraphAndRef -> NameAndPort -> Either IconGraph (String, Reference)
makePatternEdges lambdaName (_, Right patPort) lamPort =
  Left $ iconGraphFromIconsEdges mempty
    [Edge (lamPort, qualifyNameAndPort lambdaName patPort) noEnds]
makePatternEdges _ (_, Left str) lamPort = Right (str, Right lamPort)
-- TODO handle inner function definitions.

evalMatch' :: EvalContext -> Match -> State IDState IconGraph
evalMatch' c (Match _ name patterns _ rhs _) = do
  lambdaName <- getUniqueName "lam"
  let
    nameString = nameToString name
    extraVars = [(nameString, justName lambdaName)]
    (patternStringMap, patternStrings, numParameters) =
      processPatterns lambdaName patterns extraVars
  -- TODO remove coerceExpressionResult here
  rhsVal@(rhsGraph, _) <- coerceExpressionResult <$> evalRhs rhs (patternStrings <> c)
  resultIconName <- getUniqueName "res"
  rhsDrawingName <- DIA.toName <$> getUniqueName "rhsDraw"
  let
    rhsDrawing = makeRhsDrawing resultIconName rhsVal
    icons = toNames [
      (lambdaName, LambdaRegionIcon numParameters rhsDrawingName)
      --(nameString, TextBoxIcon nameString)
      ]
    --externalEdges = [Edge (justName nameString, justName lambdaName) noEnds]
    (internalEdges, unmatchedBoundVars) =
      makeInternalEdges lambdaName rhsGraph patternStrings patternStringMap
    drawing = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)]
      unmatchedBoundVars [(nameString, Right $ justName lambdaName)]
  pure drawing


-- TODO handle inner function definitions.
-- TODO: Make sure that any remaining sinks are qualified.
evalMatch :: EvalContext -> Match -> State IDState IconGraph
evalMatch c (Match _ name patterns _ rhs _) = do
  lambdaName <- getUniqueName "lam"
  patternVals <- mapM evalPattern patterns
  let
    matchFunNameString = nameToString name
    patternStrings = concatMap namesInPattern patternVals
    rhsContext = matchFunNameString : patternStrings <> c
    lambdaPorts = map (nameAndPort lambdaName) [0,1..]
    patternGraph = mconcat $ map fst patternVals

    (patternEdgeGraphs, rawNewBinds) =
      partitionEithers $ zipWith (makePatternEdges lambdaName) patternVals lambdaPorts
    patternEdgeGraph = mconcat patternEdgeGraphs

    lambdaNameRef = Right $ justName lambdaName
    newBinds = (matchFunNameString, lambdaNameRef): rawNewBinds
    numParameters = length patterns
  -- TODO remove coerceExpressionResult here
  (rhsRawGraph, rhsResult) <- coerceExpressionResult <$> evalRhs rhs rhsContext
  resultIconName <- getUniqueName "res"
  rhsDrawingName <- DIA.toName <$> getUniqueName "rhsDraw"
  let
    rhsAndPatternGraph@(IconGraph _ _ _ sinks _) = makeEdges $ patternGraph <> rhsRawGraph
    qualifiedSinks = fmap (fmap (qualifyNameAndPort lambdaName)) sinks
    (IconGraph _ internalEdges _ newSinks _) = makeEdges (IconGraph mempty mempty mempty qualifiedSinks newBinds)
    rhsDrawing = makeRhsDrawing resultIconName (rhsAndPatternGraph, rhsResult)
    icons = toNames [(lambdaName, LambdaRegionIcon numParameters rhsDrawingName)]
    finalGraph = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)]
      newSinks [(matchFunNameString, lambdaNameRef)]
  pure $ patternEdgeGraph <> finalGraph


evalMatches :: EvalContext -> [Match] -> State IDState IconGraph
evalMatches _ [] = pure mempty
evalMatches c [match] = evalMatch c match
-- TODO turn more than one match into a case expression.

-- TODO: Use the context in evalPatBind and evalMatches
evalDecl :: EvalContext -> Decl -> State IDState IconGraph
evalDecl c d = evaluatedDecl where
  evaluatedDecl = case d of
    pat@PatBind{} -> evalPatBind c pat
    FunBind matches -> evalMatches c matches

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
