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
  Stmt(..), Binds(..), Alt(..), Module(..), SpecialCon(..))
import qualified Language.Haskell.Exts as Exts
import Control.Monad.State(State, evalState)
import Data.Either(partitionEithers)
import Data.List(unzip4, partition)
import Control.Monad(replicateM)

import Types(Drawing(..), NameAndPort(..), IDState,
  initialIdState, Edge)
import Util(toNames, makeSimpleEdge, nameAndPort, justName, mapFst)
import Icons(Icon(..))
import TranslateCore(Reference, IconGraph(..), EvalContext, GraphAndRef,
  iconGraphFromIcons, iconGraphFromIconsEdges, getUniqueName, combineExpressions,
  edgesForRefPortList, iconGraphToDrawing, qualifyNameAndPort, makeApplyGraph,
  namesInPattern, lookupReference, deleteBindings, makeEdges, makeEdgesCore,
  coerceExpressionResult, makeBox, nTupleString, nListString)

-- OVERVIEW --
-- The core functions and data types used in this module are in TranslateCore.
-- The TranslateCore also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.
-- * Please note that this files uses both DIA.Name from Diagrams.Prelude, and Name from Language.Haskell.Exts

nameToString :: Language.Haskell.Exts.Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

qNameToString :: QName -> String
qNameToString (Qual (Exts.ModuleName modName) name) = modName ++ "." ++ nameToString name
qNameToString (UnQual name) = nameToString name
qNameToString (Special UnitCon) = "()"
qNameToString (Special ListCon) = "[]"
qNameToString (Special FunCon) = "(->)"
qNameToString (Special (TupleCon _ n)) = nTupleString n
qNameToString (Special Cons) = "(:)"
-- unboxed singleton tuple constructor
qNameToString (Special UnboxedSingleCon) = "(# #)"

evalPApp :: QName -> [Pat] -> State IDState (IconGraph, NameAndPort)
evalPApp name [] = makeBox $ qNameToString name
evalPApp name patterns = do
  patName <- DIA.toName <$> getUniqueName "pat"
  evaledPatterns <- mapM evalPattern patterns
  let
    constructorName = qNameToString name
    gr = makeTextApplyGraph True patName constructorName evaledPatterns (length evaledPatterns)
  pure gr


evalPLit :: Exts.Sign -> Exts.Literal -> State IDState (IconGraph, NameAndPort)
evalPLit Exts.Signless l = evalLit l
evalPLit Exts.Negative l = makeBox ('-' : showLiteral l)

evalPAsPat :: Name -> Pat -> State IDState GraphAndRef
evalPAsPat n p = do
  (evaledPatGraph, evaledPatRef) <- evalPattern p
  let
    newBind = [(nameToString n, evaledPatRef)]
    newGraph = IconGraph mempty mempty mempty mempty newBind
  pure (newGraph <> evaledPatGraph, evaledPatRef)

evalPattern :: Pat -> State IDState GraphAndRef
evalPattern p = case p of
  PVar n -> pure (mempty, Left $ nameToString n)
  PLit s l -> fmap Right <$> evalPLit s l
  PInfixApp p1 qName p2 -> evalPattern (PApp qName [p1, p2])
  PApp name patterns -> fmap Right <$> evalPApp name patterns
  -- TODO special tuple handling.
  PTuple _ patterns ->
    fmap Right <$> evalPApp (Exts.UnQual . Ident . nTupleString . length $ patterns) patterns
  PList patterns ->
    fmap Right <$> evalPApp (Exts.UnQual . Ident . nListString . length $ patterns) patterns
  PParen pat -> evalPattern pat
  PAsPat n subPat -> evalPAsPat n subPat
  PWildCard -> fmap Right <$> makeBox "_"
  _ -> error $ "evalPattern: No pattern in case for " ++ show p
  -- TODO: Other cases

-- strToGraphRef is not in TranslateCore, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> State IDState (IconGraph, Reference)
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if str `elem` c
    then (mempty, Left str)
    else fmap Right gr

evalQName :: QName -> EvalContext -> State IDState (IconGraph, Reference)
evalQName qName@(UnQual _) c = strToGraphRef c (qNameToString qName)
evalQName qName@(Qual _ _) c = strToGraphRef c (qNameToString qName)
evalQName qName _ = fmap Right <$> makeBox (qNameToString qName)

evalQOp :: QOp -> EvalContext -> State IDState (IconGraph, Reference)
evalQOp (QVarOp n) = evalQName n
evalQOp (QConOp n) = evalQName n

qOpToString :: QOp -> String
qOpToString (QVarOp n) = qNameToString n
qOpToString (QConOp n) = qNameToString n

makeTextApplyGraph :: Bool -> DIA.Name -> String -> [(IconGraph, Reference)] -> Int -> (IconGraph, NameAndPort)
makeTextApplyGraph inPattern applyIconName funStr argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    combinedGraph = combineExpressions inPattern $ zip argVals argumentPorts
    icon = if inPattern
      then PAppIcon
      else TextApplyAIcon
    icons = [(applyIconName, icon numArgs funStr)]
    newGraph = iconGraphFromIcons icons

evalApp :: EvalContext -> (Exp, [Exp]) -> State IDState (IconGraph, NameAndPort)
evalApp c exps@(funExp, argExps) = case funExp of
  (Var n) -> makeTextApp n
  (Con n) -> makeTextApp n
  _ -> evalAppNoText c exps
  where
    makeTextApp funName = let funStr = qNameToString funName in
      if funStr `elem` c
      then evalAppNoText c exps
      else do
        argVals <- mapM (evalExp c) argExps
        applyIconName <- DIA.toName <$> getUniqueName "app0"
        pure $ makeTextApplyGraph False applyIconName funStr argVals (length argExps)

evalAppNoText :: EvalContext -> (Exp, [Exp]) -> State IDState (IconGraph, NameAndPort)
evalAppNoText c (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  pure $ makeApplyGraph False applyIconName funVal argVals (length argExps)

qOpToExp :: QOp -> Exp
qOpToExp (QVarOp n) = Var n
qOpToExp (QConOp n) = Con n

evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState (IconGraph, NameAndPort)
evalInfixApp c e1 (QVarOp (UnQual (Symbol "$"))) e2 = evalApp c (e1, [e2])
evalInfixApp c e1 op e2 = evalApp c (qOpToExp op, [e1, e2])

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
  pure (newGraph, NameAndPort guardName (Just 1))

-- This is in Translate and not Translate core since currently it is only used by evalLit.
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

getBoundVarName :: Decl -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _) = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind (Match _ name _ _ _ _:_)) = [nameToString name]
-- TODO: Other cases
getBoundVarName (TypeSig _ _ _) = []
getBoundVarName decl = error $ "getBoundVarName: No pattern in case for " ++ show decl

--TODO: Should this call makeEdges?
evalBinds :: EvalContext -> Binds -> State IDState (IconGraph, EvalContext)
evalBinds c (BDecls decls) = do
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  evaledDecl <- mconcat <$> mapM (evalDecl augmentedContext) decls
  pure (evaledDecl, augmentedContext)

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
  let
    grWithEdges = makeEdges (rhsGraph <> patGraph)
    -- The pattern and rhs are conneted if makeEdges added extra edges.
    patRhsAreConnected =
      length (igEdges grWithEdges) > (length (igEdges rhsGraph) + length (igEdges patGraph))
  pure (patRhsAreConnected, deleteBindings grWithEdges, patRef, rhsRef)

-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: EvalContext -> Exts.Alt -> State IDState (Bool, IconGraph, Reference, NameAndPort)
evalAlt c (Exts.Alt _ pat rhs maybeBinds) = evalPatAndRhs c pat rhs maybeBinds

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
        rhsNewEdges = [makeSimpleEdge (rhsPort, justName resultIconName)]
    caseResultGraphs = mconcat $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
    filteredRhsEdges = mapFst Right $ fmap snd unConnectedRhss
    patternEdgesGraph = edgesForRefPortList True patEdges
    caseEdgeGraph = edgesForRefPortList False (expEdge : filteredRhsEdges)
    finalGraph = mconcat [patternEdgesGraph, caseResultGraphs, expGraph, caseEdgeGraph, caseGraph, combindedAltGraph]
  pure (finalGraph, nameAndPort caseIconName 1)

evalTuple :: EvalContext -> [Exp] -> State IDState (IconGraph, NameAndPort)
evalTuple c exps = do
  argVals <- mapM (evalExp c) exps
  applyIconName <- DIA.toName <$> getUniqueName "tupleApp"
  pure $ makeTextApplyGraph False applyIconName (nTupleString (length exps)) argVals (length exps)

makeVarExp = Var . UnQual . Ident

evalListExp :: EvalContext -> [Exp] -> State IDState (IconGraph, NameAndPort)
evalListExp c [] = makeBox "[]"
evalListExp c exps = evalApp c (makeVarExp . nListString . length $ exps, exps)

evalLeftSection :: EvalContext -> Exp -> QOp -> State IDState (IconGraph, NameAndPort)
evalLeftSection c e op = evalApp c (qOpToExp op, [e])

evalRightSection:: EvalContext -> QOp -> Exp -> State IDState (IconGraph, NameAndPort)
evalRightSection c op e = do
  expVal <- evalExp c e
  applyIconName <- DIA.toName <$> getUniqueName "tupleApp"
  -- TODO: A better option would be for makeApplyGraph to take the list of expressions as Maybes.
  neverUsedPort <- Left <$> getUniqueName "unusedArgument"
  pure $ makeTextApplyGraph False applyIconName (qOpToString op) [(mempty, neverUsedPort), expVal] 2

-- evalEnums is only used by evalExp
evalEnums :: EvalContext -> String -> [Exp] -> State IDState (IconGraph, Reference)
evalEnums c s exps = fmap Right <$> evalApp c (Var . UnQual . Ident $ s, exps)

makeQVarOp = QVarOp . UnQual . Ident

desugarDo :: [Stmt] -> Exp
desugarDo [Qualifier e] = e
desugarDo (Qualifier e : stmts) = InfixApp e thenOp (desugarDo stmts)
  where thenOp = makeQVarOp ">>"
desugarDo (Generator srcLoc pat e : stmts) =
  InfixApp e  (makeQVarOp ">>=") (Lambda srcLoc [pat] (desugarDo stmts))
desugarDo (LetStmt binds : stmts) = Let binds (desugarDo stmts)

-- TODO: Finish evalRecConstr
evalRecConstr :: EvalContext -> QName -> [Exts.FieldUpdate] -> State IDState (IconGraph, Reference)
evalRecConstr c qName updates = evalQName qName c

evalExp :: EvalContext  -> Exp -> State IDState (IconGraph, Reference)
evalExp c x = case x of
  Var n -> evalQName n c
  Con n -> evalQName n c
  Lit l -> fmap Right <$> evalLit l
  InfixApp e1 op e2 -> fmap Right <$> evalInfixApp c e1 op e2
  e@(App _ _) -> fmap Right <$> evalApp c (simplifyApp e)
  NegApp e -> evalExp c (App (makeVarExp "negate") e)
  Lambda _ patterns e -> fmap Right <$> evalLambda c patterns e
  Let bs e -> evalLet c bs e
  If e1 e2 e3 -> fmap Right <$> evalIf c e1 e2 e3
  Case e alts -> fmap Right <$> evalCase c e alts
  Do stmts -> evalExp c (desugarDo stmts)
  -- TODO special tuple symbol
  Tuple _ exps -> fmap Right <$> evalTuple c exps
  List exps -> fmap Right <$> evalListExp c exps
  Paren e -> evalExp c e
  LeftSection e op -> fmap Right <$> evalLeftSection c e op
  RightSection op e -> fmap Right <$> evalRightSection c op e
  RecConstr n updates -> evalRecConstr c n updates
  -- TODO: Do RecUpdate correcly
  RecUpdate e updates -> evalExp c e
  EnumFrom e -> evalEnums c "enumFrom" [e]
  EnumFromTo e1 e2 -> evalEnums c "enumFromTo" [e1, e2]
  EnumFromThen e1 e2 -> evalEnums c "enumFromThen" [e1, e2]
  EnumFromThenTo e1 e2 e3 -> evalEnums c "enumFromThenTo" [e1, e2, e3]
  -- TODO: Add the type signiture to ExpTypeSig.
  ExpTypeSig _ e _ -> evalExp c e
  -- TODO: Add other cases
  _ -> error $ "evalExp: No pattern in case for " ++ show x

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
        -- TODO This edge/sink should have a special arrow head to indicate an input to a pattern.
        (Left rhsStr) -> (mempty, [(rhsStr, patPort)], mempty)
        (Right rhsPort) -> ([makeSimpleEdge (rhsPort, patPort)], mempty, mempty)
    gr = IconGraph mempty newEdges mempty newSinks bindings
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

generalEvalLambda :: EvalContext -> [Pat] -> (EvalContext -> State IDState GraphAndRef) -> State IDState (IconGraph, NameAndPort)
generalEvalLambda context patterns rhsEvalFun = do
  lambdaName <- getUniqueName "lam"
  patternVals <- mapM evalPattern patterns
  let
    patternStrings = concatMap namesInPattern patternVals
    rhsContext = patternStrings <> context
    lambdaPorts = map (nameAndPort lambdaName) [2,3..]
    patternGraph = mconcat $ map fst patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith (makePatternEdges lambdaName) patternVals lambdaPorts
    numParameters = length patterns
  -- TODO remove coerceExpressionResult here
  (rhsRawGraph, rhsResult) <- rhsEvalFun rhsContext >>= coerceExpressionResult
  let
    icons = toNames [(lambdaName, FlatLambdaIcon numParameters)]
    resultIconEdge = makeSimpleEdge (rhsResult, nameAndPort lambdaName 0)
    finalGraph = IconGraph icons (resultIconEdge:patternEdges) mempty
      mempty newBinds
  pure (deleteBindings . makeEdges $ (rhsRawGraph <> patternGraph <> finalGraph), nameAndPort lambdaName 1)
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter ports.
    makePatternEdges :: String -> GraphAndRef -> NameAndPort -> Either Edge (String, Reference)
    makePatternEdges lambdaName (_, Right patPort) lamPort =
      Left $ makeSimpleEdge (lamPort, patPort)
    makePatternEdges _ (_, Left str) lamPort = Right (str, Right lamPort)


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

-- Only used by matchesToCase
matchToAlt :: Match -> Alt
matchToAlt (Match srcLocation _ mtaPats _ rhs binds) = Alt srcLocation altPattern rhs binds where
  altPattern = case mtaPats of
    [onePat] -> onePat
    _ -> PTuple Exts.Boxed mtaPats

matchesToCase :: Match -> [Match] -> State IDState Match
matchesToCase match [] = pure match
matchesToCase firstMatch@(Match srcLoc funName pats mType _ _) restOfMatches = do
  tempStrings <- replicateM (length pats) (getUniqueName "_tempvar")
  let
    tempPats = fmap (PVar . Ident) tempStrings
    tempVars = fmap (Var . UnQual . Ident) tempStrings
    tuple = Tuple Exts.Boxed tempVars
    caseExp = case tempVars of
      [oneTempVar] -> Case oneTempVar alts
      _ -> Case tuple alts
    rhs = UnGuardedRhs caseExp
    match = Match srcLoc funName tempPats mType rhs Nothing
  pure match
  where
    allMatches = firstMatch:restOfMatches
    alts = fmap matchToAlt allMatches


evalMatches :: EvalContext -> [Match] -> State IDState IconGraph
evalMatches _ [] = pure mempty
evalMatches c (firstMatch:restOfMatches) = matchesToCase firstMatch restOfMatches >>= evalMatch c

evalDecl :: EvalContext -> Decl -> State IDState IconGraph
evalDecl c d = evaluatedDecl where
  evaluatedDecl = case d of
    pat@(PatBind _ _ _ _) -> evalPatBind c pat
    FunBind matches -> evalMatches c matches
    --TODO: Add other cases here
    _ -> pure mempty

drawingFromDecl :: Decl -> Drawing
drawingFromDecl d = iconGraphToDrawing $ evalState evaluatedDecl initialIdState
  where
    evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds
    showTopLevelBinds :: IconGraph -> State IDState IconGraph
    showTopLevelBinds gr@(IconGraph _ _ _ _ binds) = do
      let
        addBind (_, Left _) = pure mempty
        addBind (patName, Right port) = do
          uniquePatName <- getUniqueName patName
          let
            icons = toNames [(uniquePatName, TextBoxIcon patName)]
            edges = [makeSimpleEdge (justName uniquePatName, port)]
            edgeGraph = iconGraphFromIconsEdges icons edges
          pure edgeGraph
      newGraph <- mconcat <$> mapM addBind binds
      pure $ newGraph <> gr

-- Profiling: about 1.5% of total time.
translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  drawing = drawingFromDecl decl

drawingsFromModule :: Module -> [Drawing]
drawingsFromModule (Module _ _ _ _ _ _ decls) = fmap drawingFromDecl decls
