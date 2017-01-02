{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TupleSections #-}
module Translate(
  translateStringToSyntaxGraph,
  translateStringToCollapsedGraphAndDecl,
  translateModuleToCollapsedGraphs
) where

import Diagrams.Prelude((<>))

import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import Data.Either(partitionEithers)
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(unzip5, partition, intercalate)
import Data.Maybe(catMaybes, isJust, fromMaybe)

import qualified Language.Haskell.Exts as Exts

import Language.Haskell.Exts(Decl(..), parseDeclWithMode, Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), fromParseResult, Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..), Alt(..), Module(..), SpecialCon(..), prettyPrint)

import GraphAlgorithms(collapseNodes)
import TranslateCore(Reference, SyntaxGraph(..), EvalContext, GraphAndRef(..), SgSink(..), SgBind(..),
  syntaxGraphFromNodes, syntaxGraphFromNodesEdges, getUniqueName, combineExpressions,
  edgesForRefPortList, makeApplyGraph, makeGuardGraph,
  namesInPattern, lookupReference, deleteBindings, makeEdges,
  makeBox, nTupleString, nTupleSectionString, nListString,
  syntaxGraphToFglGraph, getUniqueString, bindsToSyntaxGraph, graphAndRefToGraph,
  initialIdState)
import Types(NameAndPort(..), IDState,
  Edge, SyntaxNode(..), IngSyntaxGraph, NodeName, SgNamedNode(..),
  LikeApplyFlavor(..))
import Util(makeSimpleEdge, nameAndPort, justName)
import Icons(inputPort, resultPort, argumentPorts, caseRhsPorts,
             casePatternPorts)

-- OVERVIEW --
-- The core functions and data types used in this module are in TranslateCore.
-- The TranslateCore also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

-- BEGIN Helper Functions --

makeVarExp :: String -> Exp
makeVarExp = Var . UnQual . Ident

makeQVarOp :: String -> QOp
makeQVarOp = QVarOp . UnQual . Ident

qOpToExp :: QOp -> Exp
qOpToExp (QVarOp n) = Var n
qOpToExp (QConOp n) = Con n

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@) names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames = bindsToSyntaxGraph $ catMaybes $ fmap makeBind asNames where
  makeBind mName = case mName of
    Nothing -> Nothing
    Just asName -> Just $ SgBind asName ref

grNamePortToGrRef :: (SyntaxGraph, NameAndPort) -> GraphAndRef
grNamePortToGrRef (graph, np) = GraphAndRef graph (Right np)

bindOrAltHelper ::
  EvalContext -> Pat -> Rhs -> Maybe Binds -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
bindOrAltHelper c pat rhs maybeWhereBinds = do
  patGraphAndRef <- evalPattern pat
  let
    rhsContext = namesInPattern patGraphAndRef <> c
  rhsGraphAndRef <- rhsWithBinds maybeWhereBinds rhs rhsContext
  pure (patGraphAndRef, rhsGraphAndRef)

patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> ""
  )
  mStr

-- END Helper Functions --

-- BEGIN Names helper functions --

nameToString :: Exts.Name -> String
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

-- END Names helper functions

-- BEGIN evalLit

-- This is in Translate and not Translate core since currently it is only used by evalLit.
makeLiteral :: (Show x) => x -> State IDState (SyntaxGraph, NameAndPort)
makeLiteral = makeBox . show

evalLit :: Exts.Literal -> State IDState (SyntaxGraph, NameAndPort)
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

-- END evalLit

-- BEGIN evalPattern

-- BEGIN evalPApp
asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just $ SgBind asName ref

patternArgumentMapper :: ((GraphAndRef, Maybe String), t) -> (String, Either (GraphAndRef, t) (SgNamedNode, SyntaxGraph))
patternArgumentMapper (asGraphAndRef@(graphAndRef, _), port) = (patName, eitherVal)
  where
    graph = graphAndRefToGraph graphAndRef
    patName = patternName asGraphAndRef

    eitherVal = case graph of
      (SyntaxGraph [namedNode] [] _ _ _) -> Right (namedNode, graph)
      _ -> Left (graphAndRef, port)


graphToTuple :: SyntaxGraph -> ([SgNamedNode], [Edge], [SgSink], [SgBind], [(NodeName, NodeName)])
graphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)

graphsToComponents :: [SyntaxGraph] -> ([SgNamedNode], [Edge], [SgSink], [SgBind], [(NodeName, NodeName)])
graphsToComponents graphs = (concat a, concat b, concat c, concat d, concat e) where
  (a, b, c, d, e) = unzip5 $ fmap graphToTuple graphs

makeNestedPatternGraph :: NodeName -> String -> [(GraphAndRef, Maybe String)] -> (SyntaxGraph, NameAndPort)
makeNestedPatternGraph applyIconName funStr argVals = nestedApplyResult
  where
    dummyNode = NestedPatternApplyNode "" []

    argsAndPorts = zip argVals $ map (nameAndPort applyIconName) $ argumentPorts dummyNode
    mappedArgs = fmap patternArgumentMapper argsAndPorts

    (unnestedArgsAndPort, nestedNamedNodesAndGraphs) = partitionEithers (fmap snd mappedArgs)

    (nestedArgs, _, nestedSinks, nestedBinds, nestedEMaps) = graphsToComponents $ fmap snd nestedNamedNodesAndGraphs

    argListMapper (str, arg) = case arg of
      Left _ -> (Nothing, str)
      Right (namedNode, _) -> (Just namedNode, str)

    argList = fmap argListMapper mappedArgs

    combinedGraph = combineExpressions True unnestedArgsAndPort

    pAppNode = NestedPatternApplyNode funStr argList
    icons = [SgNamedNode applyIconName pAppNode]

    asNameBinds = catMaybes $ fmap asNameBind argVals
    allBinds = nestedBinds <> asNameBinds

    newEMap = ((\(SgNamedNode n _) -> (n, applyIconName))  <$> nestedArgs) <> nestedEMaps

    newGraph = SyntaxGraph icons [] nestedSinks allBinds newEMap
    nestedApplyResult = (newGraph <> combinedGraph, nameAndPort applyIconName (resultPort pAppNode))

makePatternGraph' :: NodeName -> String -> [GraphAndRef] -> (SyntaxGraph, NameAndPort)
makePatternGraph' applyIconName funStr argVals = (newGraph <> combinedGraph, nameAndPort applyIconName (resultPort pAppNode))
  where
    pAppNode = PatternApplyNode funStr numArgs
    argumentNamePorts = map (nameAndPort applyIconName) $ argumentPorts pAppNode
    combinedGraph = combineExpressions True $ zip argVals argumentNamePorts
    numArgs = length argVals
    icons = [SgNamedNode applyIconName pAppNode]
    newGraph = syntaxGraphFromNodes icons

evalPApp :: QName -> [Pat] -> State IDState (SyntaxGraph, NameAndPort)
evalPApp name patterns = case patterns of
  [] -> makeBox constructorName
  _ ->  do
    patName <- getUniqueName
    evaledPatterns <- mapM evalPattern patterns
    pure $ makeNestedPatternGraph patName constructorName evaledPatterns
  where
    constructorName = qNameToString name
-- END evalPApp

-- BEGIN evalPLit
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

evalPLit :: Exts.Sign -> Exts.Literal -> State IDState (SyntaxGraph, NameAndPort)
evalPLit sign l = case sign of
  Exts.Signless -> evalLit l
  Exts.Negative -> makeBox ('-' : showLiteral l)
-- END evalPLit

evalPAsPat :: Name -> Pat -> State IDState (GraphAndRef, Maybe String)
evalPAsPat n p = do
  (GraphAndRef evaledPatGraph evaledPatRef, mInnerName) <- evalPattern p
  let
    outerName = nameToString n
    asBindGraph = makeAsBindGraph (Left outerName) [mInnerName]
  pure (GraphAndRef (asBindGraph <> evaledPatGraph) evaledPatRef, Just outerName)

makePatternResult :: Functor f => f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

evalPattern :: Pat -> State IDState (GraphAndRef, Maybe String)
evalPattern p = case p of
  PVar n -> pure (GraphAndRef mempty (Left $ nameToString n), Nothing)
  PLit s l -> makePatternResult $ evalPLit s l
  PInfixApp p1 qName p2 -> evalPattern (PApp qName [p1, p2])
  PApp name patterns -> makePatternResult $ evalPApp name patterns
  -- TODO special tuple handling.
  PTuple _ patterns ->
    makePatternResult $ evalPApp (Exts.UnQual . Ident . nTupleString . length $ patterns) patterns
  PList patterns ->
    makePatternResult $ evalPApp (Exts.UnQual . Ident . nListString . length $ patterns) patterns
  PParen pat -> evalPattern pat
  PAsPat n subPat -> evalPAsPat n subPat
  PWildCard -> makePatternResult $ makeBox "_"
  _ -> error $ "evalPattern: No pattern in case for " ++ show p
  -- TODO: Other cases

-- END evalPattern

-- BEGIN evalQName

-- strToGraphRef is not in TranslateCore, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> State IDState GraphAndRef
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if str `elem` c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

evalQName :: QName -> EvalContext -> State IDState GraphAndRef
evalQName qName c = case qName of
  UnQual _ -> graphRef
  Qual _ _ -> graphRef
  _ -> grNamePortToGrRef <$> makeBox qNameString
  where
    qNameString = qNameToString qName
    graphRef = strToGraphRef c qNameString

-- END evalQName


-- evalQOp :: QOp -> EvalContext -> State IDState GraphAndRef
-- evalQOp (QVarOp n) = evalQName n
-- evalQOp (QConOp n) = evalQName n

-- qOpToString :: QOp -> String
-- qOpToString (QVarOp n) = qNameToString n
-- qOpToString (QConOp n) = qNameToString n

--findReferencedIcon :: Reference -> [(NodeName, Icon)] -> Maybe (Name, Icon)
-- findReferencedIcon :: Either t NameAndPort -> [(NodeName, t1)] -> Maybe (NodeName, t1)
-- findReferencedIcon (Left str) _ = Nothing
-- findReferencedIcon (Right (NameAndPort name _)) nameIconMap = (\x -> (name, x)) <$> lookup name nameIconMap


-- BEGIN apply and compose helper functions

removeParen :: Exp -> Exp
removeParen e = case e of
  Paren x -> removeParen x
  _ -> e

evalFunExpAndArgs :: EvalContext -> LikeApplyFlavor -> (Exp, [Exp]) -> State IDState (SyntaxGraph, NameAndPort)
evalFunExpAndArgs c flavor (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure $ makeApplyGraph (length argExps) flavor False applyIconName funVal argVals

-- END apply and compose helper functions

-- BEGIN evalInfixApp

evalFunctionComposition :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalFunctionComposition c functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  applyIconName <- getUniqueName
  pure $ makeApplyGraph (length evaluatedFunctions) ComposeNodeFlavor False applyIconName
    (GraphAndRef mempty neverUsedPort) evaluatedFunctions

-- | Turn (a . b . c) into [a, b, c]
compositionToList :: Exp -> [Exp]
compositionToList e = case removeParen e of
  (InfixApp exp1  (QVarOp (UnQual (Symbol "."))) exp2) -> exp1 : compositionToList exp2
  x -> [x]

-- | In the general case, infix is converted to prefix.
-- Special cases:
-- a $ b is converted to (a b)
-- (a . b . c) uses the compose apply icon with no argument
evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState GraphAndRef
evalInfixApp c e1 op e2 = case op of
  QVarOp (UnQual (Symbol sym)) -> case sym of
    "$" -> evalExp c (App e1 e2)
    "." -> grNamePortToGrRef <$> evalFunctionComposition c (e1 : compositionToList e2)
    _ -> defaultCase
  _ -> defaultCase
  where
    defaultCase = evalExp c $ App (App (qOpToExp op) e1) e2

-- END evalInfixApp

-- BEGIN evaluateAppExpression

simplifyExp :: Exp -> Exp
simplifyExp e = case removeParen e of
  InfixApp exp1  (QVarOp (UnQual (Symbol "$"))) exp2 -> App exp1 exp2
  -- Don't convert compose to apply
  InfixApp _  (QVarOp (UnQual (Symbol "."))) _ -> e
  App (Var (UnQual (Symbol "<$>"))) arg -> App (makeVarExp "fmap") arg
  InfixApp exp1 op exp2 -> App (App (qOpToExp op) exp1) exp2
  LeftSection exp1 op -> App (qOpToExp op) exp1
  x -> x

-- | Given two expressions f and x, where f is applied to x,
-- return the nesting depth if (f x) is rendered with
-- the (normal apply icon, compose apply icon)
applyComposeScoreHelper :: Exp -> Exp -> (Int, Int)
applyComposeScoreHelper exp1 exp2 = (appScore, compScore) where
  (e1App, e1Comp) = applyComposeScore exp1
  (e2App, e2Comp) = applyComposeScore exp2

  leftApp = min e1App (1 + e1Comp)
  rightApp = 1 + min e2App e2Comp

  appScore = max leftApp rightApp

  leftComp = 1 + min e1App e1Comp
  rightComp = min (1 + e2App) e2Comp
  
  compScore = max leftComp rightComp

-- TODO Consider putting this logic in a separate "simplifyExpression" function.
-- | Returns the amount of nesting if the App is converted to (applyNode, composeNode)
applyComposeScore :: Exp -> (Int, Int)
applyComposeScore e = case simplifyExp e of
  App exp1 exp2 -> applyComposeScoreHelper exp1 exp2
  _ -> (0, 0)

-- Todo add test for this function
-- | Given an App expression, return
-- (function, list of arguments)
appExpToFuncArgs :: Exp -> (Exp, [Exp])
appExpToFuncArgs e = case simplifyExp e of
  App exp1 exp2 -> (funExp, args <> [exp2])
    where
      (funExp, args) = appExpToFuncArgs exp1
  x -> (x, [])

-- | Given and App expression, return
-- (argument, list composed functions)
appExpToArgFuncs :: Exp -> (Exp, [Exp])
appExpToArgFuncs e = case simplifyExp e of
  App exp1 exp2 -> (argExp, funcs <> [exp1])
    where
      (argExp, funcs) = appExpToArgFuncs exp2
  simpleExp -> (simpleExp, [])

removeCompose :: Exp -> Exp -> Exp
removeCompose f x = case removeParen f of
  (InfixApp f1  (QVarOp (UnQual (Symbol "."))) f2) -> App f1 $ removeCompose f2 x
  _ -> App f x

-- TODO Refactor this and all sub-expressions
evalApp :: EvalContext -> Exp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalApp c f e = if appScore <= compScore
  then evalFunExpAndArgs c ApplyNodeFlavor (appExpToFuncArgs noComposeExp)
  else evalFunExpAndArgs c ComposeNodeFlavor (appExpToArgFuncs noComposeExp)
  where
    noComposeExp = removeCompose f e
    (appScore, compScore) = applyComposeScore noComposeExp

-- END evaluateAppExpression

evalIf :: EvalContext -> Exp -> Exp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalIf c boolExp trueExp falseExp = makeGuardGraph 2
  <$>
  getUniqueName
  <*>
  -- Use (pure <$>) to put the evaluated expression in a single item list
  (pure <$> evalExp c boolExp)
  <*>
  mapM (evalExp c) [trueExp, falseExp]

-- BEGIN evalGeneralLet

getBoundVarName :: Decl -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _) = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind (Match _ name _ _ _ _:_)) = [nameToString name]
-- TODO: Other cases
getBoundVarName (TypeSig _ _ _) = []
getBoundVarName decl = error $ "getBoundVarName: No pattern in case for " ++ show decl

evalBinds :: EvalContext -> Binds -> State IDState (SyntaxGraph, EvalContext)
evalBinds c (BDecls decls) =
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  in
    ((,augmentedContext) . mconcat) <$>  mapM (evalDecl augmentedContext) decls

evalGeneralLet :: (EvalContext -> State IDState GraphAndRef) -> EvalContext -> Binds -> State IDState GraphAndRef
evalGeneralLet expOrRhsEvaler c bs = do
  (bindGraph, bindContext) <- evalBinds c bs
  expVal <- expOrRhsEvaler bindContext
  let
    GraphAndRef expGraph expResult = expVal
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
    bindings = sgBinds bindGraph
  pure $ GraphAndRef newGraph (lookupReference bindings expResult)

-- END evalGeneralLet

evalLet :: EvalContext -> Binds -> Exp -> State IDState GraphAndRef
evalLet context binds e = evalGeneralLet (`evalExp` e) context binds

-- BEGIN rhsWithBinds

evalStmt :: EvalContext -> Stmt -> State IDState GraphAndRef
evalStmt c (Qualifier e) = evalExp c e

evalStmts :: EvalContext -> [Stmt] -> State IDState GraphAndRef
evalStmts c [stmt] = evalStmt c stmt

evalGuardedRhs :: EvalContext -> GuardedRhs -> State IDState (GraphAndRef, GraphAndRef)
evalGuardedRhs c (GuardedRhs _ stmts e) = (,) <$> evalStmts c stmts <*> evalExp c e

evalGuardedRhss :: EvalContext -> [GuardedRhs] -> State IDState (SyntaxGraph, NameAndPort)
evalGuardedRhss c rhss = let
  evaledRhss = unzip <$> mapM (evalGuardedRhs c) rhss
  in
  makeGuardGraph (length rhss)
  <$>
  getUniqueName
  <*>
  fmap fst evaledRhss
  <*>
  fmap snd evaledRhss

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: EvalContext -> Rhs -> State IDState GraphAndRef
evalRhs c (UnGuardedRhs e) = evalExp c e
evalRhs c (GuardedRhss rhss) = grNamePortToGrRef <$> evalGuardedRhss c rhss

rhsWithBinds :: Maybe Binds -> Rhs -> EvalContext -> State IDState GraphAndRef
rhsWithBinds maybeWhereBinds rhs rhsContext = case maybeWhereBinds of
  Nothing -> evalRhs rhsContext rhs
  Just b -> evalGeneralLet (`evalRhs` rhs) rhsContext b

-- END rhsWithBinds

-- BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a name
evalPatAndRhs :: EvalContext -> Pat -> Rhs -> Maybe Binds -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalPatAndRhs c pat rhs maybeWhereBinds = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper c pat rhs maybeWhereBinds
  let
    grWithEdges = makeEdges (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if the rhsRef refers to a source
    -- in the pattern
    patRhsAreConnected = (rhsRef /= lookedUpRhsRef) ||
      length (sgEdges grWithEdges) > (length (sgEdges rhsGraph) + length (sgEdges patGraph))
  pure (patRhsAreConnected, deleteBindings grWithEdges, patRef, lookedUpRhsRef, mPatAsName)

-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: EvalContext -> Exts.Alt -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (Exts.Alt _ pat rhs maybeBinds) = evalPatAndRhs c pat rhs maybeBinds

evalCaseHelper ::
  Int
  -> NodeName
  -> [NodeName]
  -> GraphAndRef
  -> [(Bool, SyntaxGraph, Reference, Reference, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
evalCaseHelper numAlts caseIconName resultIconNames (GraphAndRef expGraph expRef) evaledAlts = result where
  (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
  combindedAltGraph = mconcat altGraphs
  caseNode = CaseNode numAlts
  icons = [SgNamedNode caseIconName caseNode]
  caseGraph = syntaxGraphFromNodes icons
  expEdge = (expRef, nameAndPort caseIconName (inputPort caseNode))
  patEdges = zip patRefs $ map (nameAndPort caseIconName) casePatternPorts
  rhsEdges = zip patRhsConnected $ zip rhsRefs $ map (nameAndPort caseIconName) caseRhsPorts
  (connectedRhss, unConnectedRhss) = partition fst rhsEdges

  makeCaseResult :: NodeName -> Reference -> SyntaxGraph
  makeCaseResult resultIconName rhsRef = case rhsRef of
    Left _ -> mempty
    Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
      where
        rhsNewIcons = [SgNamedNode resultIconName CaseResultNode]
        rhsNewEdges = [makeSimpleEdge (rhsPort, justName resultIconName)]

  caseResultGraphs = mconcat $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
  filteredRhsEdges = fmap snd unConnectedRhss
  patternEdgesGraph = edgesForRefPortList True patEdges
  caseEdgeGraph = edgesForRefPortList False (expEdge : filteredRhsEdges)

  bindGraph = makeAsBindGraph expRef asNames

  finalGraph = deleteBindings $ makeEdges $ mconcat [bindGraph, patternEdgesGraph, caseResultGraphs, expGraph, caseEdgeGraph, caseGraph, combindedAltGraph]
  result = (finalGraph, nameAndPort caseIconName (resultPort caseNode))


evalCase :: EvalContext -> Exp -> [Alt] -> State IDState (SyntaxGraph, NameAndPort)
evalCase c e alts =
  let
    numAlts = length alts
  in
    evalCaseHelper (length alts)
    <$>
    getUniqueName
    <*>
    replicateM numAlts getUniqueName
    <*>
    evalExp c e
    <*>
    mapM (evalAlt c) alts

-- END evalCase

evalTuple :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalTuple c exps =
  let
    numExps = length exps
  in
    makeApplyGraph numExps ApplyNodeFlavor False
    <$>
    getUniqueName
    <*>
    (grNamePortToGrRef <$> makeBox (nTupleString numExps))
    <*>
    mapM (evalExp c) exps

evalTupleSection :: EvalContext -> [Maybe Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalTupleSection c mExps =
  let
    exps = catMaybes mExps
    expIsJustList = fmap isJust mExps
  in
    makeApplyGraph (length exps) ApplyNodeFlavor False
    <$>
    getUniqueName
    <*>
    (grNamePortToGrRef <$> makeBox (nTupleSectionString expIsJustList))
    <*>
    mapM (evalExp c) exps

evalListExp :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalListExp _ [] = makeBox "[]"
evalListExp c exps = evalFunExpAndArgs c ApplyNodeFlavor (makeVarExp . nListString . length $ exps, exps)

evalLeftSection :: EvalContext -> Exp -> QOp -> State IDState GraphAndRef
evalLeftSection c e op = evalExp c $ App (qOpToExp op) e

evalRightSection :: EvalContext -> QOp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalRightSection c op e =
  makeApplyGraph 2 ApplyNodeFlavor False
  <$>
  getUniqueName
  <*>
  evalExp c (qOpToExp op)
  <*>
  ((\x y -> [x, y]) <$>
    -- TODO: A better option would be for makeApplyGraph to take the list of expressions as Maybes.
    fmap (GraphAndRef mempty . Left) (getUniqueString "unusedArgument")
    <*>
    evalExp c e
  )

-- evalEnums is only used by evalExp
evalEnums :: EvalContext -> String -> [Exp] -> State IDState GraphAndRef
evalEnums c s exps = grNamePortToGrRef <$> evalFunExpAndArgs c ApplyNodeFlavor (makeVarExp s, exps)

desugarDo :: [Stmt] -> Exp
desugarDo [Qualifier e] = e
desugarDo (Qualifier e : stmts) = InfixApp e thenOp (desugarDo stmts)
  where thenOp = makeQVarOp ">>"
desugarDo (Generator srcLoc pat e : stmts) =
  InfixApp e  (makeQVarOp ">>=") (Lambda srcLoc [pat] (desugarDo stmts))
desugarDo (LetStmt binds : stmts) = Let binds (desugarDo stmts)

-- TODO: Finish evalRecConstr
evalRecConstr :: EvalContext -> QName -> [Exts.FieldUpdate] -> State IDState GraphAndRef
evalRecConstr c qName _ = evalQName qName c

-- BEGIN generalEvalLambda

-- TODO Returning a SyntaxGraph is probably not very efficient
asBindGraphZipper :: Maybe String -> NameAndPort -> SyntaxGraph
asBindGraphZipper asName nameNPort = makeAsBindGraph (Right nameNPort) [asName]

generalEvalLambda :: EvalContext -> [Pat] -> (EvalContext -> State IDState GraphAndRef) -> State IDState (SyntaxGraph, NameAndPort)
generalEvalLambda context patterns rhsEvalFun = do
  lambdaName <- getUniqueName
  patternValsWithAsNames <- mapM evalPattern patterns
  let
    patternVals = fmap fst patternValsWithAsNames
    patternStrings = concatMap namesInPattern patternValsWithAsNames
    rhsContext = patternStrings <> context
    paramNames = fmap patternName patternValsWithAsNames
    lambdaNode = FunctionDefNode paramNames
    lambdaPorts = map (nameAndPort lambdaName) $ argumentPorts lambdaNode
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts

  GraphAndRef rhsRawGraph rhsRef <- rhsEvalFun rhsContext
  let
    icons = [SgNamedNode lambdaName lambdaNode]
    returnPort = nameAndPort lambdaName (inputPort lambdaNode)
    (newEdges, newSinks) = case rhsRef of
      Left s -> (patternEdges, [SgSink s returnPort])
      Right rhsPort ->  (makeSimpleEdge (rhsPort, returnPort) : patternEdges, mempty)
    finalGraph = SyntaxGraph icons newEdges newSinks newBinds mempty

    asBindGraph = mconcat $ zipWith asBindGraphZipper (fmap snd patternValsWithAsNames) lambdaPorts
  
  pure (deleteBindings . makeEdges $ (asBindGraph <> rhsRawGraph <> patternGraph <> finalGraph), nameAndPort lambdaName (resultPort lambdaNode))
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter ports.
    makePatternEdges :: GraphAndRef -> NameAndPort -> Either Edge SgBind
    makePatternEdges (GraphAndRef _ ref) lamPort = case ref of
      Right patPort -> Left $ makeSimpleEdge (lamPort, patPort)
      Left str -> Right $ SgBind str (Right lamPort)

-- END generalEvalLambda

evalLambda :: EvalContext -> [Pat] -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalLambda c patterns e = generalEvalLambda c patterns (`evalExp` e)

evalExp :: EvalContext  -> Exp -> State IDState GraphAndRef
evalExp c x = case x of
  Var n -> evalQName n c
  Con n -> evalQName n c
  Lit l -> grNamePortToGrRef <$> evalLit l
  InfixApp e1 op e2 -> evalInfixApp c e1 op e2
  App f arg -> grNamePortToGrRef <$> evalApp c f arg
  NegApp e -> evalExp c (App (makeVarExp "negate") e)
  Lambda _ patterns e -> grNamePortToGrRef <$> evalLambda c patterns e
  Let bs e -> evalLet c bs e
  If e1 e2 e3 -> grNamePortToGrRef <$> evalIf c e1 e2 e3
  Case e alts -> grNamePortToGrRef <$> evalCase c e alts
  Do stmts -> evalExp c (desugarDo stmts)
  -- TODO special tuple symbol
  Tuple _ exps -> grNamePortToGrRef <$> evalTuple c exps
  TupleSection _ mExps -> grNamePortToGrRef <$> evalTupleSection c mExps
  List exps -> grNamePortToGrRef <$> evalListExp c exps
  Paren e -> evalExp c e
  LeftSection e op -> evalLeftSection c e op
  RightSection op e -> grNamePortToGrRef <$> evalRightSection c op e
  RecConstr n updates -> evalRecConstr c n updates
  -- TODO: Do RecUpdate correcly
  RecUpdate e _ -> evalExp c e
  EnumFrom e -> evalEnums c "enumFrom" [e]
  EnumFromTo e1 e2 -> evalEnums c "enumFromTo" [e1, e2]
  EnumFromThen e1 e2 -> evalEnums c "enumFromThen" [e1, e2]
  EnumFromThenTo e1 e2 e3 -> evalEnums c "enumFromThenTo" [e1, e2, e3]
  -- TODO: Add the type signiture to ExpTypeSig.
  ExpTypeSig _ e _ -> evalExp c e
  -- TODO: Add other cases
  _ -> error $ "evalExp: No pattern in case for " ++ show x

-- BEGIN evalDecl

-- BEGIN evalMatches

-- Only used by matchesToCase
matchToAlt :: Match -> Alt
matchToAlt (Match srcLocation _ mtaPats _ rhs binds) = Alt srcLocation altPattern rhs binds where
  altPattern = case mtaPats of
    [onePat] -> onePat
    _ -> PTuple Exts.Boxed mtaPats

matchesToCase :: Match -> [Match] -> State IDState Match
matchesToCase match [] = pure match
matchesToCase firstMatch@(Match srcLoc funName pats mType _ _) restOfMatches = do
  -- There is a special case in Icons.hs/makeLabelledPort to exclude " tempvar"
  tempStrings <- replicateM (length pats) (getUniqueString " tempvar")
  let
    tempPats = fmap (PVar . Ident) tempStrings
    tempVars = fmap makeVarExp tempStrings
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

evalMatch :: EvalContext -> Match -> State IDState SyntaxGraph
evalMatch c (Match _ name patterns _ rhs maybeWhereBinds) = do
  let
    matchFunNameString = nameToString name
    newContext = matchFunNameString : c
  (lambdaGraph, lambdaPort) <-
    generalEvalLambda newContext patterns (rhsWithBinds maybeWhereBinds rhs)
  let
    newBinding = bindsToSyntaxGraph [SgBind matchFunNameString (Right lambdaPort)]
  pure $ makeEdges (newBinding <> lambdaGraph)

evalMatches :: EvalContext -> [Match] -> State IDState SyntaxGraph
evalMatches _ [] = pure mempty
evalMatches c (firstMatch:restOfMatches) = matchesToCase firstMatch restOfMatches >>= evalMatch c

-- END evalMatches

evalPatBind :: EvalContext -> Decl -> State IDState SyntaxGraph
evalPatBind c (PatBind _ pat rhs maybeWhereBinds) = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper c pat rhs maybeWhereBinds
  let
    (newEdges, newSinks, bindings) = case patRef of
      (Left s) -> (mempty, mempty, [SgBind s rhsRef])
      (Right patPort) -> case rhsRef of
        (Left rhsStr) -> (mempty, [SgSink rhsStr patPort], mempty)
        (Right rhsPort) -> ([makeSimpleEdge (rhsPort, patPort)], mempty, mempty)
    asBindGraph = makeAsBindGraph rhsRef [mPatAsName]
    gr = asBindGraph <> SyntaxGraph mempty newEdges newSinks bindings mempty
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

-- Pretty printing the entire type sig results in extra whitespace in the middle
-- TODO May want to trim whitespace from (prettyPrint typeForNames)
evalTypeSig :: Decl -> State IDState (SyntaxGraph, NameAndPort)
evalTypeSig (TypeSig _ names typeForNames) = makeBox
  (intercalate "," (fmap prettyPrint names)
   ++ " :: "
   ++ prettyPrint typeForNames)

evalDecl :: EvalContext -> Decl -> State IDState SyntaxGraph
evalDecl c d = case d of
    PatBind _ _ _ _ -> evalPatBind c d
    FunBind matches -> evalMatches c matches
    TypeSig _ _ _ -> fst <$> evalTypeSig d
    --TODO: Add other cases here
    _ -> pure mempty

-- END evalDecl

-- BEGIN Exported functions

showTopLevelBinds :: SyntaxGraph -> State IDState SyntaxGraph
showTopLevelBinds gr = do
  let
    binds = sgBinds gr
    addBind (SgBind _ (Left _)) = pure mempty
    addBind (SgBind patName (Right port)) = do
      uniquePatName <- getUniqueName
      let
        icons = [SgNamedNode uniquePatName (BindNameNode patName)]
        edges = [makeSimpleEdge (port, justName uniquePatName)]
        edgeGraph = syntaxGraphFromNodesEdges icons edges
      pure edgeGraph
  newGraph <- mconcat <$> mapM addBind binds
  pure $ newGraph <> gr

translateDeclToSyntaxGraph :: Decl -> SyntaxGraph
translateDeclToSyntaxGraph d = graph where
  evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

customParseMode :: Exts.ParseMode
customParseMode = Exts.defaultParseMode
  {Exts.extensions =
   [Exts.EnableExtension Exts.MultiParamTypeClasses,
    Exts.EnableExtension Exts.FlexibleContexts,
    Exts.EnableExtension Exts.TupleSections
   ]
  }

customParseDecl :: String -> Decl
customParseDecl = fromParseResult . parseDeclWithMode customParseMode

-- | Convert a single function declaration into a SyntaxGraph
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . customParseDecl

syntaxGraphToCollapsedGraph :: SyntaxGraph -> IngSyntaxGraph FGR.Gr
syntaxGraphToCollapsedGraph = collapseNodes . syntaxGraphToFglGraph

translateDeclToCollapsedGraph :: Decl -> IngSyntaxGraph FGR.Gr
translateDeclToCollapsedGraph = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph

-- Profiling: At one point, this was about 1.5% of total time.
translateStringToCollapsedGraphAndDecl :: String -> (IngSyntaxGraph FGR.Gr, Decl)
translateStringToCollapsedGraphAndDecl s = (drawing, decl) where
  decl = customParseDecl s -- :: ParseResult Module
  drawing = translateDeclToCollapsedGraph decl

translateModuleToCollapsedGraphs :: Module -> [IngSyntaxGraph FGR.Gr]
translateModuleToCollapsedGraphs (Module _ _ _ _ _ _ decls) = fmap translateDeclToCollapsedGraph decls

-- END Exported functions
