{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module Translate(
  translateStringToSyntaxGraph,
  translateStringToCollapsedGraphAndDecl,
  translateModuleToCollapsedGraphs,
  qOpToExp,
  qNameToString,
  matchesToCase,
  customParseDecl
) where

import Diagrams.Prelude((<>))

import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import Data.Either(partitionEithers)
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List(unzip5, partition, intercalate)
import Data.Maybe(catMaybes, isJust, fromMaybe)

import qualified Language.Haskell.Exts as Exts

import Language.Haskell.Exts(
  Decl(..), Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..), Alt(..), Module(..), prettyPrint)

import GraphAlgorithms(collapseNodes)
import Icons(inputPort, resultPort, argumentPorts, caseRhsPorts,
             casePatternPorts)
import SimplifySyntax(stringToSimpDecl, SimpExp(..), SimpPat(..), qOpToExp
                     , qNameToString, nameToString, customParseDecl
                     , SimpDecl(..), hsDeclToSimpDecl)
import TranslateCore(
  Reference, SyntaxGraph(..), EvalContext, GraphAndRef(..), SgSink(..),
  syntaxGraphFromNodes, syntaxGraphFromNodesEdges, getUniqueName,
  edgesForRefPortList, makeApplyGraph, makeGuardGraph, combineExpressions,
  namesInPattern, lookupReference, deleteBindings, makeEdges,
  makeBox, nTupleString, nTupleSectionString, nListString,
  syntaxGraphToFglGraph, getUniqueString, bindsToSyntaxGraph,
  SgBind(..), graphAndRefToGraph,
  initialIdState)
import Types(Labeled(..), NameAndPort(..), IDState,
  Edge, SyntaxNode(..), IngSyntaxGraph, NodeName, SgNamedNode(..),
  LikeApplyFlavor(..))
import Util(makeSimpleEdge, nameAndPort, justName)

{-# ANN module "HLint: ignore Use record patterns" #-}

-- OVERVIEW --
-- The core functions and data types used in this module are in TranslateCore.
-- The TranslateCore also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

-- BEGIN Helper Functions --

makeVarExp :: l -> String -> Exp l
makeVarExp l  = Var l . UnQual l . Ident l

makeQVarOp :: l -> String -> QOp l
makeQVarOp l = QVarOp l . UnQual l . Ident l

-- | Make a syntax graph that has the bindings for a list of "as pattern" (@)
-- names.
makeAsBindGraph :: Reference -> [Maybe String] -> SyntaxGraph
makeAsBindGraph ref asNames
  = bindsToSyntaxGraph $ catMaybes $ fmap makeBind asNames
  where
    makeBind mName = case mName of
      Nothing -> Nothing
      Just asName -> Just $ SgBind asName ref

grNamePortToGrRef :: (SyntaxGraph, NameAndPort) -> GraphAndRef
grNamePortToGrRef (graph, np) = GraphAndRef graph (Right np)

bindOrAltHelper :: Show l =>
  EvalContext
  -> Pat l
  -> Rhs l
  -> Maybe (Binds l)
  -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
bindOrAltHelper c pat rhs maybeWhereBinds = do
  patGraphAndRef <- evalPattern pat
  let
    rhsContext = namesInPattern patGraphAndRef <> c
  rhsGraphAndRef <- rhsWithBinds maybeWhereBinds rhs rhsContext
  pure (patGraphAndRef, rhsGraphAndRef)

-- TODO Find a better name for bindOrAltHelper
bindOrAltHelper' :: Show l =>
  EvalContext
  -> SimpPat l
  -> SimpExp l
  -> State IDState ((GraphAndRef, Maybe String), GraphAndRef)
bindOrAltHelper' c pat e = do
  patGraphAndRef <- evalPattern' pat
  let
    rhsContext = namesInPattern patGraphAndRef <> c
  rhsGraphAndRef <- evalExp' rhsContext e
  pure (patGraphAndRef, rhsGraphAndRef)


patternName :: (GraphAndRef, Maybe String) -> String
patternName (GraphAndRef _ ref, mStr) = fromMaybe
  (case ref of
    Left str -> str
    Right _ -> ""
  )
  mStr

-- END Helper Functions --

-- BEGIN evalLit

-- This is in Translate and not Translate core since currently it is only used
-- by evalLit.
makeLiteral :: (Show x) => x -> State IDState (SyntaxGraph, NameAndPort)
makeLiteral = makeBox . show

evalLit :: Exts.Literal l -> State IDState (SyntaxGraph, NameAndPort)
evalLit (Exts.Int _ x _) = makeLiteral x
evalLit (Exts.Char _ x _) = makeLiteral x
evalLit (Exts.String _ x _) = makeLiteral x
-- TODO: Print the Rational as a floating point.
evalLit (Exts.Frac _ x _) = makeLiteral x
-- TODO: Test the unboxed literals
evalLit (Exts.PrimInt _ x _) = makeLiteral x
evalLit (Exts.PrimWord _ x _) = makeLiteral x
evalLit (Exts.PrimFloat _ x _) = makeLiteral x
evalLit (Exts.PrimDouble _ x _) = makeLiteral x
evalLit (Exts.PrimChar _ x _) = makeLiteral x
evalLit (Exts.PrimString _ x _) = makeLiteral x

-- END evalLit

-- BEGIN evalPattern

-- BEGIN evalPApp
asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just $ SgBind asName ref

patternArgumentMapper ::
  ((GraphAndRef, Maybe String), t)
  -> (String, Either (GraphAndRef, t) (SgNamedNode, SyntaxGraph))
patternArgumentMapper (asGraphAndRef@(graphAndRef, _), port)
  = (patName, eitherVal)
  where
    graph = graphAndRefToGraph graphAndRef
    patName = patternName asGraphAndRef

    eitherVal = case graph of
      (SyntaxGraph [namedNode] [] _ _ _) -> Right (namedNode, graph)
      _ -> Left (graphAndRef, port)


graphToTuple ::
  SyntaxGraph
  -> ([SgNamedNode], [Edge], [SgSink], [SgBind], [(NodeName, NodeName)])
graphToTuple (SyntaxGraph a b c d e) = (a, b, c, d, e)

graphsToComponents ::
  [SyntaxGraph]
  -> ([SgNamedNode], [Edge], [SgSink], [SgBind], [(NodeName, NodeName)])
graphsToComponents graphs = (concat a, concat b, concat c, concat d, concat e)
  where
    (a, b, c, d, e) = unzip5 $ fmap graphToTuple graphs

makeNestedPatternGraph ::
  NodeName
  -> String
  -> [(GraphAndRef, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
makeNestedPatternGraph applyIconName funStr argVals = nestedApplyResult
  where
    dummyNode = NestedPatternApplyNode "" []

    argsAndPorts
      = zip argVals $ map (nameAndPort applyIconName) $ argumentPorts dummyNode
    mappedArgs = fmap patternArgumentMapper argsAndPorts

    (unnestedArgsAndPort, nestedNamedNodesAndGraphs)
      = partitionEithers (fmap snd mappedArgs)

    (nestedArgs, _, nestedSinks, nestedBinds, nestedEMaps)
      = graphsToComponents $ fmap snd nestedNamedNodesAndGraphs

    argListMapper (str, arg) = case arg of
      Left _ -> Labeled Nothing str
      Right (namedNode, _) -> Labeled (Just namedNode) str

    argList = fmap argListMapper mappedArgs

    combinedGraph = combineExpressions True unnestedArgsAndPort

    pAppNode = NestedPatternApplyNode funStr argList
    icons = [SgNamedNode applyIconName pAppNode]

    asNameBinds = catMaybes $ fmap asNameBind argVals
    allBinds = nestedBinds <> asNameBinds

    newEMap = ((\(SgNamedNode n _) -> (n, applyIconName))  <$> nestedArgs)
              <> nestedEMaps

    newGraph = SyntaxGraph icons [] nestedSinks allBinds newEMap
    nestedApplyResult = (newGraph <> combinedGraph
                        , nameAndPort applyIconName (resultPort pAppNode))


evalPApp :: Show l =>
  QName l
  -> [Pat l]
  -> State IDState (SyntaxGraph, NameAndPort)
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
showLiteral :: Exts.Literal l -> String
showLiteral (Exts.Int _ x _) = show x
showLiteral (Exts.Char _ x _) = show x
showLiteral (Exts.String _ x _) = show x
-- TODO: Print the Rational as a floating point.
showLiteral (Exts.Frac _ x _) = show x
-- TODO: Test the unboxed literals
showLiteral (Exts.PrimInt _ x _) = show x
showLiteral (Exts.PrimWord _ x _) = show x
showLiteral (Exts.PrimFloat _ x _) = show x
showLiteral (Exts.PrimDouble _ x _) = show x
showLiteral (Exts.PrimChar _ x _) = show x
showLiteral (Exts.PrimString _ x _) = show x

evalPLit ::
  Exts.Sign l -> Exts.Literal l -> State IDState (SyntaxGraph, NameAndPort)
evalPLit sign l = case sign of
  Exts.Signless _ -> evalLit l
  Exts.Negative _ -> makeBox ('-' : showLiteral l)
-- END evalPLit

evalPAsPat :: Show l =>
  Name l -> Pat l -> State IDState (GraphAndRef, Maybe String)
evalPAsPat n p = do
  (GraphAndRef evaledPatGraph evaledPatRef, mInnerName) <- evalPattern p
  let
    outerName = nameToString n
    asBindGraph = makeAsBindGraph (Left outerName) [mInnerName]
  pure (GraphAndRef (asBindGraph <> evaledPatGraph) evaledPatRef
       , Just outerName)

makePatternResult :: Functor f =>
  f (SyntaxGraph, NameAndPort) -> f (GraphAndRef, Maybe String)
makePatternResult
  = fmap (\(graph, namePort) -> (GraphAndRef graph (Right namePort), Nothing))

evalPattern :: Show l => Pat l -> State IDState (GraphAndRef, Maybe String)
evalPattern p = case p of
  PVar _ n -> pure (GraphAndRef mempty (Left $ nameToString n), Nothing)
  PLit _ s l -> makePatternResult $ evalPLit s l
  PInfixApp l p1 qName p2 -> evalPattern (PApp l qName [p1, p2])
  PApp _ name patterns -> makePatternResult $ evalPApp name patterns
  -- TODO special tuple handling.
  PTuple l _ patterns ->
    makePatternResult $ evalPApp
                (Exts.UnQual l . Ident l . nTupleString . length $ patterns)
                patterns
  PList l patterns ->
    makePatternResult $ evalPApp
                (Exts.UnQual l . Ident l . nListString . length $ patterns)
                patterns
  PParen _ pat -> evalPattern pat
  PAsPat _ n subPat -> evalPAsPat n subPat
  PWildCard _ -> makePatternResult $ makeBox "_"
  _ -> error $ "evalPattern: No pattern in case for " ++ show p
  -- TODO: Other cases

evalPattern' :: Show l => SimpPat l -> State IDState (GraphAndRef, Maybe String)
evalPattern' p = case p of
  SpVar _ n -> pure (GraphAndRef mempty (Left $ nameToString n), Nothing)

-- END evalPattern

-- BEGIN evalQName

-- strToGraphRef is not in TranslateCore, since it is only used by evalQName.
strToGraphRef :: EvalContext -> String -> State IDState GraphAndRef
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if str `elem` c
    then GraphAndRef mempty (Left str)
    else grNamePortToGrRef gr

evalQName :: Show l => QName l -> EvalContext -> State IDState GraphAndRef
evalQName qName c = case qName of
  UnQual _ _ -> graphRef
  Qual _ _ _ -> graphRef
  _ -> grNamePortToGrRef <$> makeBox qNameString
  where
    qNameString = qNameToString qName
    graphRef = strToGraphRef c qNameString

-- END evalQName

-- TODO Delete these commented out functions.
-- evalQOp :: QOp l -> EvalContext -> State IDState GraphAndRef
-- evalQOp (QVarOp n) = evalQName n
-- evalQOp (QConOp n) = evalQName n

-- qOpToString :: QOp l -> String
-- qOpToString (QVarOp n) = qNameToString n
-- qOpToString (QConOp n) = qNameToString n

--findReferencedIcon :: Reference -> [(NodeName, Icon)] -> Maybe (Name, Icon)
-- findReferencedIcon :: Either t NameAndPort -> [(NodeName, t1)] -> Maybe (NodeName, t1)
-- findReferencedIcon (Left str) _ = Nothing
-- findReferencedIcon (Right (NameAndPort name _)) nameIconMap = (\x -> (name, x)) <$> lookup name nameIconMap


-- BEGIN apply and compose helper functions

removeParen :: Exp l -> Exp l
removeParen e = case e of
  Paren _ x -> removeParen x
  _ -> e

evalFunExpAndArgs :: Show l =>
  EvalContext
  -> LikeApplyFlavor
  -> (Exp l, [Exp l])
  -> State IDState (SyntaxGraph, NameAndPort)
evalFunExpAndArgs c flavor (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure
    $ makeApplyGraph (length argExps) flavor False applyIconName funVal argVals

-- END apply and compose helper functions

-- BEGIN evalInfixApp

evalFunctionComposition :: Show l =>
  EvalContext -> [Exp l] -> State IDState (SyntaxGraph, NameAndPort)
evalFunctionComposition c functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  applyIconName <- getUniqueName
  pure $ makeApplyGraph
    (length evaluatedFunctions)
    ComposeNodeFlavor
    False
    applyIconName
    (GraphAndRef mempty neverUsedPort)
    evaluatedFunctions

-- | Turn (a . b . c) into [a, b, c]
compositionToList :: Exp l -> [Exp l]
compositionToList e = case removeParen e of
  (InfixApp _ exp1  (QVarOp _ (UnQual _ (Symbol _ "."))) exp2)
    -> exp1 : compositionToList exp2
  x -> [x]

-- | In the general case, infix is converted to prefix.
-- Special cases:
-- a $ b is converted to (a b)
-- (a . b . c) uses the compose apply icon with no argument
evalInfixApp :: Show l =>
  l -> EvalContext -> Exp l -> QOp l -> Exp l -> State IDState GraphAndRef
evalInfixApp l c e1 op e2 = case op of
  QVarOp _ (UnQual _ (Symbol _ sym)) -> case sym of
    "$" -> evalExp c (App l e1 e2)
    "." -> grNamePortToGrRef
           <$> evalFunctionComposition c (e1 : compositionToList e2)
    _ -> defaultCase
  _ -> defaultCase
  where
    defaultCase = evalExp c $ App l (App l (qOpToExp op) e1) e2

-- END evalInfixApp

-- BEGIN evaluateAppExpression

simplifyExp :: Exp l -> Exp l
simplifyExp e = case removeParen e of
  InfixApp l exp1  (QVarOp _ (UnQual _ (Symbol _ "$"))) exp2 -> App l exp1 exp2
  -- Don't convert compose to apply
  InfixApp _ _ (QVarOp _ (UnQual _ (Symbol _ "."))) _ -> e
  App l (Var _ (UnQual _ (Symbol _ "<$>"))) arg
    -> App l (makeVarExp l "fmap") arg
  InfixApp l exp1 op exp2 -> App l (App l (qOpToExp op) exp1) exp2
  LeftSection l exp1 op -> App l (qOpToExp op) exp1
  x -> x

-- | Given two expressions f and x, where f is applied to x,
-- return the nesting depth if (f x) is rendered with
-- the (normal apply icon, compose apply icon)
applyComposeScoreHelper :: Exp l -> Exp l -> (Int, Int)
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
-- | Returns the amount of nesting if the App is converted to
-- (applyNode, composeNode)
applyComposeScore :: Exp l -> (Int, Int)
applyComposeScore e = case simplifyExp e of
  App _ exp1 exp2 -> applyComposeScoreHelper exp1 exp2
  _ -> (0, 0)

-- Todo add test for this function
-- | Given an App expression, return
-- (function, list of arguments)
appExpToFuncArgs :: Exp l -> (Exp l, [Exp l])
appExpToFuncArgs e = case simplifyExp e of
  App _ exp1 exp2 -> (funExp, args <> [exp2])
    where
      (funExp, args) = appExpToFuncArgs exp1
  x -> (x, [])

-- | Given and App expression, return
-- (argument, list composed functions)
appExpToArgFuncs :: Exp l -> (Exp l, [Exp l])
appExpToArgFuncs e = case simplifyExp e of
  App _ exp1 exp2 -> (argExp, funcs <> [exp1])
    where
      (argExp, funcs) = appExpToArgFuncs exp2
  simpleExp -> (simpleExp, [])

removeCompose :: l -> Exp l -> Exp l -> Exp l
removeCompose l f x = case removeParen f of
  (InfixApp _ f1  (QVarOp _ (UnQual _ (Symbol _ "."))) f2)
    -> App l f1 $ removeCompose l f2 x
  _ -> App l f x

-- TODO Refactor this and all sub-expressions
evalApp :: Show l =>
  l -> EvalContext -> Exp l -> Exp l -> State IDState (SyntaxGraph, NameAndPort)
evalApp l c f e = if appScore <= compScore
  then evalFunExpAndArgs c ApplyNodeFlavor (appExpToFuncArgs noComposeExp)
  else evalFunExpAndArgs c ComposeNodeFlavor (appExpToArgFuncs noComposeExp)
  where
    noComposeExp = removeCompose l f e
    (appScore, compScore) = applyComposeScore noComposeExp

-- END evaluateAppExpression

evalIf :: Show l =>
  EvalContext
  -> Exp l
  -> Exp l
  -> Exp l
  -> State IDState (SyntaxGraph, NameAndPort)
evalIf c boolExp trueExp falseExp = makeGuardGraph 2
  <$>
  getUniqueName
  <*>
  -- Use (pure <$>) to put the evaluated expression in a single item list
  (pure <$> evalExp c boolExp)
  <*>
  mapM (evalExp c) [trueExp, falseExp]

-- BEGIN evalGeneralLet

getBoundVarName :: Show l => Decl l -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _)
  = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind _ (Match _ name _ _ _:_)) = [nameToString name]
-- TODO: Other cases
getBoundVarName (TypeSig _ _ _) = []
getBoundVarName decl
  = error $ "getBoundVarName: No pattern in case for " ++ show decl

evalBinds :: Show l =>
  EvalContext -> Binds l -> State IDState (SyntaxGraph, EvalContext)
evalBinds c (BDecls _ decls) =
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  in
    (,augmentedContext) . mconcat <$> mapM (evalDecl augmentedContext) decls
evalBinds _ binds = error $ "Unsupported syntax in evalBinds: " <> show binds

evalGeneralLet :: Show l =>
  (EvalContext -> State IDState GraphAndRef)
  -> EvalContext
  -> Binds l
  -> State IDState GraphAndRef
evalGeneralLet expOrRhsEvaler c bs = do
  (bindGraph, bindContext) <- evalBinds c bs
  expVal <- expOrRhsEvaler bindContext
  let
    GraphAndRef expGraph expResult = expVal
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
    bindings = sgBinds bindGraph
  pure $ GraphAndRef newGraph (lookupReference bindings expResult)

-- END evalGeneralLet

evalLet :: Show l => EvalContext -> Binds l -> Exp l-> State IDState GraphAndRef
evalLet context binds e = evalGeneralLet (`evalExp` e) context binds

-- BEGIN rhsWithBinds

evalStmt :: Show l => EvalContext -> Stmt l -> State IDState GraphAndRef
evalStmt c (Qualifier _ e) = evalExp c e
evalStmt _ q = error $ "Unsupported syntax in evalStmt: " <> show q

evalStmts :: Show l => EvalContext -> [Stmt l] -> State IDState GraphAndRef
evalStmts c [stmt] = evalStmt c stmt
evalStmts _ stmts = error $ "Unsupported syntax in evalStmts: " <> show stmts

evalGuardedRhs :: Show l =>
  EvalContext -> GuardedRhs l -> State IDState (GraphAndRef, GraphAndRef)
evalGuardedRhs c (GuardedRhs _ stmts e)
  = (,) <$> evalStmts c stmts <*> evalExp c e

evalGuardedRhss :: Show l =>
  EvalContext -> [GuardedRhs l] -> State IDState (SyntaxGraph, NameAndPort)
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
evalRhs :: Show l => EvalContext -> Rhs l -> State IDState GraphAndRef
evalRhs c (UnGuardedRhs _ e) = evalExp c e
evalRhs c (GuardedRhss _ rhss) = grNamePortToGrRef <$> evalGuardedRhss c rhss

rhsWithBinds :: Show l =>
  Maybe (Binds l) -> Rhs l -> EvalContext -> State IDState GraphAndRef
rhsWithBinds maybeWhereBinds rhs rhsContext = case maybeWhereBinds of
  Nothing -> evalRhs rhsContext rhs
  Just b -> evalGeneralLet (`evalRhs` rhs) rhsContext b

-- END rhsWithBinds

-- BEGIN evalCase

-- TODO patRhsAreConnected is sometimes incorrectly true if the pat is just a
-- name
evalPatAndRhs :: Show l =>
  EvalContext
  -> Pat l
  -> Rhs l
  -> Maybe (Binds l)
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalPatAndRhs c pat rhs maybeWhereBinds = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper c pat rhs maybeWhereBinds
  let
    grWithEdges = makeEdges (rhsGraph <> patGraph)
    lookedUpRhsRef = lookupReference (sgBinds grWithEdges) rhsRef
    -- The pattern and rhs are conneted if makeEdges added extra edges, or if
    -- the rhsRef refers to a source in the pattern.
    patRhsAreConnected
      = (rhsRef /= lookedUpRhsRef)
        || ( length (sgEdges grWithEdges)
             >
             (length (sgEdges rhsGraph) + length (sgEdges patGraph)))
  pure (patRhsAreConnected
       , deleteBindings grWithEdges
       , patRef
       , lookedUpRhsRef
       , mPatAsName)

-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: Show l =>
  EvalContext
  -> Exts.Alt l
  -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalAlt c (Exts.Alt _ pat rhs maybeBinds) = evalPatAndRhs c pat rhs maybeBinds

evalCaseHelper ::
  Int
  -> NodeName
  -> [NodeName]
  -> GraphAndRef
  -> [(Bool, SyntaxGraph, Reference, Reference, Maybe String)]
  -> (SyntaxGraph, NameAndPort)
evalCaseHelper numAlts caseIconName resultIconNames
  (GraphAndRef expGraph expRef) evaledAlts
  = result
  where
    (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    caseNode = CaseNode numAlts
    icons = [SgNamedNode caseIconName caseNode]
    caseGraph = syntaxGraphFromNodes icons
    expEdge = (expRef, nameAndPort caseIconName (inputPort caseNode))
    patEdges = zip patRefs $ map (nameAndPort caseIconName) casePatternPorts
    rhsEdges = zip patRhsConnected $ zip rhsRefs
               $ map (nameAndPort caseIconName) caseRhsPorts
    (connectedRhss, unConnectedRhss) = partition fst rhsEdges

    makeCaseResult :: NodeName -> Reference -> SyntaxGraph
    makeCaseResult resultIconName rhsRef = case rhsRef of
      Left _ -> mempty
      Right rhsPort -> syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
        where
          rhsNewIcons = [SgNamedNode resultIconName CaseResultNode]
          rhsNewEdges = [makeSimpleEdge (rhsPort, justName resultIconName)]

    caseResultGraphs =
      mconcat
      $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
    filteredRhsEdges = fmap snd unConnectedRhss
    patternEdgesGraph = edgesForRefPortList True patEdges
    caseEdgeGraph = edgesForRefPortList False (expEdge : filteredRhsEdges)

    bindGraph = makeAsBindGraph expRef asNames

    finalGraph = deleteBindings $ makeEdges $ mconcat [bindGraph
                                                      , patternEdgesGraph
                                                      , caseResultGraphs
                                                      , expGraph
                                                      , caseEdgeGraph
                                                      , caseGraph
                                                      , combindedAltGraph]
    result = (finalGraph, nameAndPort caseIconName (resultPort caseNode))


evalCase :: Show l =>
  EvalContext -> Exp l -> [Alt l] -> State IDState (SyntaxGraph, NameAndPort)
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

evalTuple :: Show l =>
  EvalContext -> [Exp l] -> State IDState (SyntaxGraph, NameAndPort)
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

evalTupleSection :: Show l =>
  EvalContext -> [Maybe (Exp l)] -> State IDState (SyntaxGraph, NameAndPort)
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

evalListExp :: Show l =>
  l -> EvalContext -> [Exp l] -> State IDState (SyntaxGraph, NameAndPort)
evalListExp _ _ [] = makeBox "[]"
evalListExp l c exps = evalFunExpAndArgs
                       c
                       ApplyNodeFlavor
                       (makeVarExp l . nListString . length $ exps, exps)

evalLeftSection :: Show l =>
  l -> EvalContext -> Exp l -> QOp l -> State IDState GraphAndRef
evalLeftSection l c e op = evalExp c $ App l (qOpToExp op) e

evalRightSection :: Show l =>
  EvalContext -> QOp l -> Exp l -> State IDState (SyntaxGraph, NameAndPort)
evalRightSection c op e =
  makeApplyGraph 2 ApplyNodeFlavor False
  <$>
  getUniqueName
  <*>
  evalExp c (qOpToExp op)
  <*>
  ((\x y -> [x, y]) <$>
    -- TODO: A better option would be for makeApplyGraph to take the list of
    -- expressions as Maybes.
    fmap (GraphAndRef mempty . Left) (getUniqueString "unusedArgument")
    <*>
    evalExp c e
  )

-- evalEnums is only used by evalExp
evalEnums :: Show l =>
  l -> EvalContext -> String -> [Exp l] -> State IDState GraphAndRef
evalEnums l c s exps
  = grNamePortToGrRef
    <$> evalFunExpAndArgs c ApplyNodeFlavor (makeVarExp l s, exps)

desugarDo :: Show l => [Stmt l] -> Exp l
desugarDo [Qualifier _ e] = e
desugarDo (Qualifier l e : stmts) = InfixApp l e thenOp (desugarDo stmts)
  where thenOp = makeQVarOp l ">>"
desugarDo (Generator l pat e : stmts) =
  InfixApp l e (makeQVarOp l ">>=") (Lambda l [pat] (desugarDo stmts))
desugarDo (LetStmt l binds : stmts) = Let l binds (desugarDo stmts)
desugarDo stmts = error $ "Unsupported syntax in degugarDo: " <> show stmts

-- TODO: Finish evalRecConstr
evalRecConstr :: Show l =>
  EvalContext -> QName l -> [Exts.FieldUpdate l] -> State IDState GraphAndRef
evalRecConstr c qName _ = evalQName qName c

-- BEGIN generalEvalLambda

-- TODO Returning a SyntaxGraph is probably not very efficient
asBindGraphZipper :: Maybe String -> NameAndPort -> SyntaxGraph
asBindGraphZipper asName nameNPort = makeAsBindGraph (Right nameNPort) [asName]

generalEvalLambda :: Show l
  => EvalContext
  -> [Pat l]
  -> (EvalContext -> State IDState GraphAndRef)
  -> State IDState (SyntaxGraph, NameAndPort)
generalEvalLambda context patterns rhsEvalFun = do
  lambdaName <- getUniqueName
  patternValsWithAsNames <- mapM evalPattern patterns
  let
    patternVals = fmap fst patternValsWithAsNames
    patternStrings = concatMap namesInPattern patternValsWithAsNames
    rhsContext = patternStrings <> context
  GraphAndRef rhsRawGraph rhsRef <- rhsEvalFun rhsContext
  let
    paramNames = fmap patternName patternValsWithAsNames
    enclosedNodeNames = snnName <$> sgNodes combinedGraph
    lambdaNode = FunctionDefNode paramNames enclosedNodeNames
    lambdaPorts = map (nameAndPort lambdaName) $ argumentPorts lambdaNode
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts

    icons = [SgNamedNode lambdaName lambdaNode]
    returnPort = nameAndPort lambdaName (inputPort lambdaNode)
    (newEdges, newSinks) = case rhsRef of
      Left s -> (patternEdges, [SgSink s returnPort])
      Right rhsPort ->
        (makeSimpleEdge (rhsPort, returnPort) : patternEdges, mempty)
    finalGraph = SyntaxGraph icons newEdges newSinks newBinds mempty

    asBindGraph = mconcat $ zipWith
                  asBindGraphZipper
                  (fmap snd patternValsWithAsNames)
                  lambdaPorts
    combinedGraph = deleteBindings . makeEdges
                    $ (asBindGraph <> rhsRawGraph <> patternGraph <> finalGraph)

  pure (combinedGraph, nameAndPort lambdaName (resultPort lambdaNode))
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the
    -- input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter
    -- ports.
    makePatternEdges :: GraphAndRef -> NameAndPort -> Either Edge SgBind
    makePatternEdges (GraphAndRef _ ref) lamPort = case ref of
      Right patPort -> Left $ makeSimpleEdge (lamPort, patPort)
      Left str -> Right $ SgBind str (Right lamPort)

-- TODO Refactor evalLambda
evalLambda' :: Show l
  => l
  -> EvalContext
  -> [SimpPat l]
  -> SimpExp l
  -> State IDState (SyntaxGraph, NameAndPort)
evalLambda' _ context patterns expr = do
  lambdaName <- getUniqueName
  patternValsWithAsNames <- mapM evalPattern' patterns
  let
    patternVals = fmap fst patternValsWithAsNames
    patternStrings = concatMap namesInPattern patternValsWithAsNames
    rhsContext = patternStrings <> context
  GraphAndRef rhsRawGraph rhsRef <- evalExp' rhsContext expr
  let
    paramNames = fmap patternName patternValsWithAsNames
    enclosedNodeNames = snnName <$> sgNodes combinedGraph
    lambdaNode = FunctionDefNode paramNames enclosedNodeNames
    lambdaPorts = map (nameAndPort lambdaName) $ argumentPorts lambdaNode
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts

    icons = [SgNamedNode lambdaName lambdaNode]
    returnPort = nameAndPort lambdaName (inputPort lambdaNode)
    (newEdges, newSinks) = case rhsRef of
      Left s -> (patternEdges, [SgSink s returnPort])
      Right rhsPort ->
        (makeSimpleEdge (rhsPort, returnPort) : patternEdges, mempty)
    finalGraph = SyntaxGraph icons newEdges newSinks newBinds mempty

    asBindGraph = mconcat $ zipWith
                  asBindGraphZipper
                  (fmap snd patternValsWithAsNames)
                  lambdaPorts
    combinedGraph = deleteBindings . makeEdges
                    $ (asBindGraph <> rhsRawGraph <> patternGraph <> finalGraph)

  pure (combinedGraph, nameAndPort lambdaName (resultPort lambdaNode))
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the
    -- input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter
    -- ports.
    makePatternEdges :: GraphAndRef -> NameAndPort -> Either Edge SgBind
    makePatternEdges (GraphAndRef _ ref) lamPort = case ref of
      Right patPort -> Left $ makeSimpleEdge (lamPort, patPort)
      Left str -> Right $ SgBind str (Right lamPort)

-- END generalEvalLambda

evalLambda :: Show l =>
  EvalContext -> [Pat l] -> Exp l -> State IDState (SyntaxGraph, NameAndPort)
evalLambda c patterns e = generalEvalLambda c patterns (`evalExp` e)


evalExp :: Show l => EvalContext -> Exp l -> State IDState GraphAndRef
evalExp c x = case x of
  Var _ n -> evalQName n c
  Con _ n -> evalQName n c
  Lit _ l -> grNamePortToGrRef <$> evalLit l
  InfixApp l e1 op e2 -> evalInfixApp l c e1 op e2
  App l f arg -> grNamePortToGrRef <$> evalApp l c f arg
  NegApp l e -> evalExp c (App l (makeVarExp l "negate") e)
  Lambda _ patterns e -> grNamePortToGrRef <$> evalLambda c patterns e
  Let _ bs e -> evalLet c bs e
  If _ e1 e2 e3 -> grNamePortToGrRef <$> evalIf c e1 e2 e3
  Case _ e alts -> grNamePortToGrRef <$> evalCase c e alts
  Do _ stmts -> evalExp c (desugarDo stmts)
  -- TODO special tuple symbol
  Tuple _ _ exps -> grNamePortToGrRef <$> evalTuple c exps
  TupleSection _ _ mExps -> grNamePortToGrRef <$> evalTupleSection c mExps
  List l exps -> grNamePortToGrRef <$> evalListExp l c exps
  Paren _ e -> evalExp c e
  LeftSection l e op -> evalLeftSection l c e op
  RightSection _ op e -> grNamePortToGrRef <$> evalRightSection c op e
  RecConstr _ n updates -> evalRecConstr c n updates
  -- TODO: Do RecUpdate correcly
  RecUpdate _ e _ -> evalExp c e
  EnumFrom l e -> evalEnums l c "enumFrom" [e]
  EnumFromTo l e1 e2 -> evalEnums l c "enumFromTo" [e1, e2]
  EnumFromThen l e1 e2 -> evalEnums l c "enumFromThen" [e1, e2]
  EnumFromThenTo l e1 e2 e3 -> evalEnums l c "enumFromThenTo" [e1, e2, e3]
  -- TODO: Add the type signiture to ExpTypeSig.
  ExpTypeSig _ e _ -> evalExp c e
  -- TODO: Add other cases
  _ -> error $ "evalExp: No pattern in case for " ++ show x

evalExp' :: Show l => EvalContext -> SimpExp l -> State IDState GraphAndRef
evalExp' c x = case x of
  SeName _ s -> strToGraphRef c s
  SeLit _ lit -> grNamePortToGrRef <$> evalLit lit
  SeLambda l patterns e -> grNamePortToGrRef <$> evalLambda' l c patterns e
  _ -> error ("evalExp' todo: " <> show x)

-- BEGIN evalDecl

-- BEGIN evalMatches

-- Only used by matchesToCase
matchToAlt :: Show l => Match l -> Alt l
matchToAlt (Match l _ mtaPats rhs binds) = Alt l altPattern rhs binds where
  altPattern = case mtaPats of
    [onePat] -> onePat
    _ -> PTuple l Exts.Boxed mtaPats
matchToAlt match = error $ "Unsupported syntax in matchToAlt: " <> show match

matchesToCase :: Show l => Match l -> [Match l] -> Match l
matchesToCase match [] = match
matchesToCase firstMatch@(Match srcLoc funName pats _ _) restOfMatches = match
  where
    -- There is a special case in Icons.hs/makeLabelledPort to exclude " tempvar"
    -- TODO use a data constructor for the special case instead of using string
    -- matching for tempvars.
    tempStrings = fmap (\x -> " tempvar" ++ show x) [0..(length pats - 1)]
    tempPats = fmap (PVar srcLoc . Ident srcLoc) tempStrings
    tempVars = fmap (makeVarExp srcLoc) tempStrings
    tuple = Tuple srcLoc Exts.Boxed tempVars
    caseExp = case tempVars of
      [oneTempVar] -> Case srcLoc oneTempVar alts
      _ -> Case srcLoc tuple alts
    rhs = UnGuardedRhs srcLoc caseExp
    match = Match srcLoc funName tempPats rhs Nothing
    allMatches = firstMatch:restOfMatches
    alts = fmap matchToAlt allMatches
matchesToCase firstMatch _
  = error $ "Unsupported syntax in matchesToCase: " <> show firstMatch

evalMatch :: Show l => EvalContext -> Match l -> State IDState SyntaxGraph
evalMatch c (Match _ name patterns rhs maybeWhereBinds) = do
  let
    matchFunNameString = nameToString name
    newContext = matchFunNameString : c
  (lambdaGraph, lambdaPort) <-
    generalEvalLambda newContext patterns (rhsWithBinds maybeWhereBinds rhs)
  let
    newBinding
      = bindsToSyntaxGraph [SgBind matchFunNameString (Right lambdaPort)]
  pure $ makeEdges (newBinding <> lambdaGraph)
evalMatch _ match = error $ "Unsupported syntax in evalMatch: " <> show match

evalMatches :: Show l => EvalContext -> [Match l] -> State IDState SyntaxGraph
evalMatches _ [] = pure mempty
evalMatches c (firstMatch:restOfMatches)
  = evalMatch c $ matchesToCase firstMatch restOfMatches

-- END evalMatches

evalPatBind :: Show l => EvalContext -> Decl l -> State IDState SyntaxGraph
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
evalPatBind _ decl = error $ "Unsupported syntax in evalPatBind: " <> show decl

evalPatBind' :: Show l =>
  l -> EvalContext -> SimpPat l -> SimpExp l -> State IDState SyntaxGraph
evalPatBind' _ c pat e = do
  ((GraphAndRef patGraph patRef, mPatAsName), GraphAndRef rhsGraph rhsRef) <-
    bindOrAltHelper' c pat e
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
evalTypeSig :: Show l => Decl l -> State IDState (SyntaxGraph, NameAndPort)
evalTypeSig (TypeSig _ names typeForNames) = makeBox
  (intercalate "," (fmap prettyPrintWithoutNewlines names)
   ++ " :: "
   ++ prettyPrintWithoutNewlines typeForNames)
  where
    -- TODO Make custom version of prettyPrint for type signitures.
    -- Use (unwords . words) to convert consecutive whitspace characters to one
    -- space.
    prettyPrintWithoutNewlines = unwords . words . prettyPrint
evalTypeSig decl
  = error $ "Unsupported syntax in evalTypeSig: " <> show decl

evalDecl :: Show l => EvalContext -> Decl l -> State IDState SyntaxGraph
evalDecl c d = case d of
    PatBind _ _ _ _ -> evalPatBind c d
    FunBind _ matches -> evalMatches c matches
    TypeSig _ _ _ -> fst <$> evalTypeSig d
    --TODO: Add other cases here
    _ -> pure mempty

evalDecl' :: Show l => EvalContext -> SimpDecl l -> State IDState SyntaxGraph
evalDecl' c d = case d of
  SdPatBind l pat e -> evalPatBind' l c pat e
  -- TypeSig _ _ _ -> -- TODO

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

translateDeclToSyntaxGraph :: Show l => Decl l -> SyntaxGraph
translateDeclToSyntaxGraph d = graph where
  evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

translateDeclToSyntaxGraph' :: Show l => SimpDecl l -> SyntaxGraph
translateDeclToSyntaxGraph' d = graph where
  evaluatedDecl = evalDecl' mempty d >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState

-- | Convert a single function declaration into a SyntaxGraph
translateStringToSyntaxGraph' :: String -> SyntaxGraph
translateStringToSyntaxGraph' = translateDeclToSyntaxGraph . customParseDecl

translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph' . stringToSimpDecl

syntaxGraphToCollapsedGraph :: SyntaxGraph -> IngSyntaxGraph FGR.Gr
syntaxGraphToCollapsedGraph = collapseNodes . syntaxGraphToFglGraph

translateDeclToCollapsedGraph' :: Show l => Decl l -> IngSyntaxGraph FGR.Gr
translateDeclToCollapsedGraph'
  = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph

translateDeclToCollapsedGraph :: Show l => Decl l -> IngSyntaxGraph FGR.Gr
translateDeclToCollapsedGraph
  = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph' . hsDeclToSimpDecl

-- Profiling: At one point, this was about 1.5% of total time.
translateStringToCollapsedGraphAndDecl ::
  String -> (IngSyntaxGraph FGR.Gr, Decl Exts.SrcSpanInfo)
translateStringToCollapsedGraphAndDecl s = (drawing, decl) where
  decl = customParseDecl s -- :: ParseResult Module
  drawing = translateDeclToCollapsedGraph decl

translateModuleToCollapsedGraphs :: Show l =>
  Module l -> [IngSyntaxGraph FGR.Gr]
translateModuleToCollapsedGraphs (Module _ _ _ _ decls)
  = fmap translateDeclToCollapsedGraph decls
translateModuleToCollapsedGraphs moduleSyntax
  = error $ "Unsupported syntax in translateModuleToCollapsedGraphs: "
    <> show moduleSyntax

-- END Exported functions
