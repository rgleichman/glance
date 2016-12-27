{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
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
import Data.Maybe(catMaybes)
import qualified Language.Haskell.Exts as Exts

import Language.Haskell.Exts(Decl(..), parseDecl, Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), fromParseResult, Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..), Alt(..), Module(..), SpecialCon(..), prettyPrint)

import GraphAlgorithms(collapseNodes)
import TranslateCore(Reference, SyntaxGraph(..), EvalContext, GraphAndRef(..), SgSink(..), SgBind(..),
  syntaxGraphFromNodes, syntaxGraphFromNodesEdges, getUniqueName, combineExpressions,
  edgesForRefPortList, makeApplyGraph,
  namesInPattern, lookupReference, deleteBindings, makeEdges,
  makeBox, nTupleString, nListString,
  syntaxGraphToFglGraph, getUniqueString, bindsToSyntaxGraph, graphAndRefToGraph,
  initialIdState)
import Types(NameAndPort(..), IDState,
  Edge, SyntaxNode(..), IngSyntaxGraph, NodeName, Port(..), SgNamedNode(..),
  LikeApplyFlavor(..))
import Util(makeSimpleEdge, nameAndPort, justName)

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
-- TODO Refactor decideIfNested and makePatternGraph
decideIfNested ::
  (GraphAndRef, t)
  -> (Maybe (GraphAndRef, t), Maybe SgNamedNode, [SgSink], [SgBind],
      [(NodeName, NodeName)])
decideIfNested (GraphAndRef (SyntaxGraph [nameAndIcon] [] sinks bindings eMap) _ , _) = (Nothing, Just nameAndIcon, sinks, bindings, eMap)
decideIfNested valAndPort = (Just valAndPort, Nothing, [], [], [])

asNameBind :: (GraphAndRef, Maybe String) -> Maybe SgBind
asNameBind (GraphAndRef _ ref, mAsName) = case mAsName of
  Nothing -> Nothing
  Just asName -> Just $ SgBind asName ref

-- TODO Consider removing the Int numArgs parameter.
makePatternGraph :: NodeName -> String -> [(GraphAndRef, Maybe String)] -> Int -> (SyntaxGraph, NameAndPort)
makePatternGraph applyIconName funStr argVals _ = nestedApplyResult
  where
    argumentPorts = map (nameAndPort applyIconName . Port) [2,3..]
    argValsWithoutAsNames = fmap fst argVals
    (unnestedArgsAndPort, nestedArgs, nestedSinks, nestedBindings, nestedEMaps) =
      unzip5 $ fmap decideIfNested (zip argValsWithoutAsNames argumentPorts)

    asNameBinds = catMaybes $ fmap asNameBind argVals

    allSinks = mconcat nestedSinks
    allBinds = mconcat nestedBindings <> asNameBinds

    originalPortExpPairs = catMaybes unnestedArgsAndPort
    portExpressionPairs = originalPortExpPairs
    combinedGraph = combineExpressions True portExpressionPairs
    icons = [SgNamedNode applyIconName (NestedPatternApplyNode funStr nestedArgs)]
    newEMap = ((\(SgNamedNode n _) -> (n, applyIconName))  <$> catMaybes nestedArgs) <> mconcat nestedEMaps
    
    newGraph = SyntaxGraph icons [] allSinks allBinds newEMap
    nestedApplyResult = (newGraph <> combinedGraph, nameAndPort applyIconName (Port 1))

makePatternGraph' :: NodeName -> String -> [GraphAndRef] -> Int -> (SyntaxGraph, NameAndPort)
makePatternGraph' applyIconName funStr argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName (Port 1))
  where
    argumentPorts = map (nameAndPort applyIconName . Port) [2,3..]
    combinedGraph = combineExpressions True $ zip argVals argumentPorts
    icons = [SgNamedNode applyIconName (PatternApplyNode funStr numArgs)]
    newGraph = syntaxGraphFromNodes icons

evalPApp :: QName -> [Pat] -> State IDState (SyntaxGraph, NameAndPort)
evalPApp name patterns = case patterns of
  [] -> makeBox constructorName
  _ ->  do
    patName <- getUniqueName
    evaledPatterns <- mapM evalPattern patterns
    pure $ makePatternGraph patName constructorName evaledPatterns (length evaledPatterns)
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

evalApp :: EvalContext -> LikeApplyFlavor -> (Exp, [Exp]) -> State IDState (SyntaxGraph, NameAndPort)
evalApp c flavor (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName
  pure $ makeApplyGraph flavor False applyIconName funVal argVals (length argExps)

-- END apply and compose helper functions

-- BEGIN evalInfixApp

evalPureCompose :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalPureCompose c functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  applyIconName <- getUniqueName
  pure $ makeApplyGraph ComposeNodeFlavor False applyIconName
    (GraphAndRef mempty neverUsedPort) evaluatedFunctions (length evaluatedFunctions)

simplifyPureCompose :: Exp -> [Exp]
simplifyPureCompose e = case removeParen e of
  (InfixApp exp1  (QVarOp (UnQual (Symbol "."))) exp2) -> exp1 : simplifyPureCompose exp2
  x -> [x]

evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState GraphAndRef
evalInfixApp c e1 op e2 = case op of
  QVarOp (UnQual (Symbol sym)) -> case sym of
    "$" -> evalExp c (App e1 e2)
    "." -> grNamePortToGrRef <$> evalPureCompose c (e1 : simplifyPureCompose e2)
    _ -> defaultCase
  _ -> defaultCase
  where
    defaultCase = evalExp c $ App (App (qOpToExp op) e1) e2

-- END evalInfixApp

-- BEGIN evaluateAppExpression

scoreExpressions :: Exp -> Exp -> (Int, Int)
scoreExpressions exp1 exp2 = (appScore, compScore) where
  (e1App, e1Comp) = applyComposeScore exp1
  (e2App, e2Comp) = applyComposeScore exp2

  leftApp = min e1App (1 + e1Comp)
  rightApp = 1 + min e2App e2Comp

  appScore = max leftApp rightApp

  leftComp = 1 + min e1App e1Comp
  rightComp = min (1 + e2App) e2Comp
  
  compScore = max leftComp rightComp

simplifyExp :: Exp -> Exp
simplifyExp e = case removeParen e of
  InfixApp exp1  (QVarOp (UnQual (Symbol "$"))) exp2 -> App exp1 exp2
  -- Don't convert compose to apply
  InfixApp _  (QVarOp (UnQual (Symbol "."))) _ -> e
  App (Var (UnQual (Symbol "<$>"))) arg -> App (makeVarExp "fmap") arg
  InfixApp exp1 op exp2 -> App (App (qOpToExp op) exp1) exp2
  LeftSection exp1 op -> App (qOpToExp op) exp1
  x -> x

-- TODO Consider putting this logic in a separate "simplifyExpression" function.
-- | Returns the amount of nesting if the App is converted to (applyNode, composeNode)
applyComposeScore :: Exp -> (Int, Int)
applyComposeScore e = case simplifyExp e of
  App exp1 exp2 -> scoreExpressions exp1 exp2
  _ -> (0, 0)

-- Todo add test for this function
simplifyApp :: Exp -> (Exp, [Exp])
simplifyApp e = case simplifyExp e of
  App exp1 exp2 -> (funExp, args <> [exp2])
    where
      (funExp, args) = simplifyApp exp1
  x -> (x, [])

simplifyComposeApply :: Exp -> (Exp, [Exp])
simplifyComposeApply e = case simplifyExp e of
  App exp1 exp2 -> (argExp, funcs <> [exp1])
    where
      (argExp, funcs) = simplifyComposeApply exp2
  simpleExp -> (simpleExp, [])

removeCompose :: Exp -> Exp -> Exp
removeCompose f x = case removeParen f of
  (InfixApp f1  (QVarOp (UnQual (Symbol "."))) f2) -> App f1 $ removeCompose f2 x
  _ -> App f x

-- TODO Refactor this and all sub-expressions
evaluateAppExpression :: EvalContext -> Exp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evaluateAppExpression c f e = if appScore <= compScore
  then evalApp c ApplyNodeFlavor (simplifyApp noComposeExp)
  else evalApp c ComposeNodeFlavor (simplifyComposeApply noComposeExp)
  where
    noComposeExp = removeCompose f e
    (appScore, compScore) = applyComposeScore noComposeExp

-- END evaluateAppExpression

evalIf :: EvalContext -> Exp -> Exp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalIf c e1 e2 e3 = do
  e1Val <- evalExp c e1
  e2Val <- evalExp c e2
  e3Val <- evalExp c e3
  guardName <- getUniqueName
  let
    icons = [SgNamedNode guardName (GuardNode 2)]
    combinedGraph =
      combineExpressions False $ zip [e1Val, e2Val, e3Val] (map (nameAndPort guardName . Port) [3, 2, 4])
    newGraph = syntaxGraphFromNodes icons <> combinedGraph
  pure (newGraph, nameAndPort guardName (Port 1))

-- BEGIN evalGeneralLet

getBoundVarName :: Decl -> [String]
-- TODO Should evalState be used here?
getBoundVarName (PatBind _ pat _ _) = namesInPattern $ evalState (evalPattern pat) initialIdState
getBoundVarName (FunBind (Match _ name _ _ _ _:_)) = [nameToString name]
-- TODO: Other cases
getBoundVarName (TypeSig _ _ _) = []
getBoundVarName decl = error $ "getBoundVarName: No pattern in case for " ++ show decl

--TODO: Should this call makeEdges?
evalBinds :: EvalContext -> Binds -> State IDState (SyntaxGraph, EvalContext)
evalBinds c (BDecls decls) = do
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  evaledDecl <- mconcat <$> mapM (evalDecl augmentedContext) decls
  pure (evaledDecl, augmentedContext)

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

evalGuaredRhs :: EvalContext -> GuardedRhs -> State IDState (GraphAndRef, GraphAndRef)
evalGuaredRhs c (GuardedRhs _ stmts e) = do
  expVal <- evalExp c e
  stmtsVal <- evalStmts c stmts
  pure (stmtsVal, expVal)

evalGuardedRhss :: EvalContext -> [GuardedRhs] -> State IDState (SyntaxGraph, NameAndPort)
evalGuardedRhss c rhss = do
  guardName <- getUniqueName
  evaledRhss <- mapM (evalGuaredRhs c) rhss
  let
    (bools, exps) = unzip evaledRhss
    expsWithPorts = zip exps $ map (nameAndPort guardName . Port) [2,4..]
    boolsWithPorts = zip bools $ map (nameAndPort guardName . Port) [3,5..]
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [SgNamedNode guardName $ GuardNode (length rhss)]
    newGraph = syntaxGraphFromNodes icons <> combindedGraph
  pure (newGraph, nameAndPort guardName (Port 1))

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

-- TODO: Refactor this with evalPatBind
evalPatAndRhs :: EvalContext -> Pat -> Rhs -> Maybe Binds -> State IDState (Bool, SyntaxGraph, Reference, Reference, Maybe String)
evalPatAndRhs c pat rhs maybeWhereBinds = do
  patternNames <- namesInPattern <$> evalPattern pat
  let rhsContext = patternNames <> c
  GraphAndRef rhsGraph rhsRef <- rhsWithBinds maybeWhereBinds rhs rhsContext
  (GraphAndRef patGraph patRef, mPatAsName) <- evalPattern pat
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

evalCase :: EvalContext -> Exp -> [Alt] -> State IDState (SyntaxGraph, NameAndPort)
evalCase c e alts = do
  evaledAlts <- mapM (evalAlt c) alts
  GraphAndRef expGraph expRef <- evalExp c e
  caseIconName <- getUniqueName
  let
    (patRhsConnected, altGraphs, patRefs, rhsRefs, asNames) = unzip5 evaledAlts
    combindedAltGraph = mconcat altGraphs
    numAlts = length alts
    icons = [SgNamedNode caseIconName (CaseNode numAlts)]
    caseGraph = syntaxGraphFromNodes icons
    expEdge = (expRef, nameAndPort caseIconName (Port 0))
    patEdges = zip patRefs $ map (nameAndPort caseIconName . Port) [2,4..]
    rhsEdges = zip patRhsConnected $ zip rhsRefs $ map (nameAndPort caseIconName . Port) [3,5..]
    (connectedRhss, unConnectedRhss) = partition fst rhsEdges
  resultIconNames <- replicateM numAlts getUniqueName
  let
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
  pure (finalGraph, nameAndPort caseIconName (Port 1))

-- END evalCase

evalTuple :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalTuple c exps = do
  argVals <- mapM (evalExp c) exps
  funVal <- makeBox $ nTupleString (length exps)
  applyIconName <- getUniqueName
  pure $ makeApplyGraph ApplyNodeFlavor False applyIconName (grNamePortToGrRef funVal) argVals (length exps)

evalListExp :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalListExp _ [] = makeBox "[]"
evalListExp c exps = evalApp c ApplyNodeFlavor (makeVarExp . nListString . length $ exps, exps)

evalLeftSection :: EvalContext -> Exp -> QOp -> State IDState GraphAndRef
evalLeftSection c e op = evalExp c $ App (qOpToExp op) e

evalRightSection :: EvalContext -> QOp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalRightSection c op e = do
  expVal <- evalExp c e
  funVal <- evalExp c (qOpToExp op)
  applyIconName <- getUniqueName
  -- TODO: A better option would be for makeApplyGraph to take the list of expressions as Maybes.
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  pure $ makeApplyGraph ApplyNodeFlavor False applyIconName funVal [GraphAndRef mempty neverUsedPort, expVal] 2

-- evalEnums is only used by evalExp
evalEnums :: EvalContext -> String -> [Exp] -> State IDState GraphAndRef
evalEnums c s exps = grNamePortToGrRef <$> evalApp c ApplyNodeFlavor (makeVarExp s, exps)

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
    lambdaPorts = map (nameAndPort lambdaName . Port) [2,3..]
    patternGraph = mconcat $ fmap graphAndRefToGraph patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts

  GraphAndRef rhsRawGraph rhsRef <- rhsEvalFun rhsContext
  let
    icons = [SgNamedNode lambdaName $ FunctionDefNode (length patterns)]
    returnPort = nameAndPort lambdaName (Port 0)
    (newEdges, newSinks) = case rhsRef of
      Left s -> (patternEdges, [SgSink s returnPort])
      Right rhsPort ->  (makeSimpleEdge (rhsPort, returnPort) : patternEdges, mempty)
    finalGraph = SyntaxGraph icons newEdges newSinks newBinds mempty

    asBindGraph = mconcat $ zipWith asBindGraphZipper (fmap snd patternValsWithAsNames) lambdaPorts
  
  pure (deleteBindings . makeEdges $ (asBindGraph <> rhsRawGraph <> patternGraph <> finalGraph), nameAndPort lambdaName (Port 1))
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
  App f arg -> grNamePortToGrRef <$> evaluateAppExpression c f arg
  NegApp e -> evalExp c (App (makeVarExp "negate") e)
  Lambda _ patterns e -> grNamePortToGrRef <$> evalLambda c patterns e
  Let bs e -> evalLet c bs e
  If e1 e2 e3 -> grNamePortToGrRef <$> evalIf c e1 e2 e3
  Case e alts -> grNamePortToGrRef <$> evalCase c e alts
  Do stmts -> evalExp c (desugarDo stmts)
  -- TODO special tuple symbol
  Tuple _ exps -> grNamePortToGrRef <$> evalTuple c exps
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
  tempStrings <- replicateM (length pats) (getUniqueString "_tempvar")
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
  patternNames <- namesInPattern <$> evalPattern pat
  let rhsContext = patternNames <> c
  GraphAndRef rhsGraph rhsRef <- rhsWithBinds maybeWhereBinds rhs rhsContext
  (GraphAndRef patGraph patRef, patAsName) <- evalPattern pat
  let
    (newEdges, newSinks, bindings) = case patRef of
      (Left s) -> (mempty, mempty, [SgBind s rhsRef])
      (Right patPort) -> case rhsRef of
        -- TODO This edge/sink should have a special arrow head to indicate an input to a pattern.
        (Left rhsStr) -> (mempty, [SgSink rhsStr patPort], mempty)
        (Right rhsPort) -> ([makeSimpleEdge (rhsPort, patPort)], mempty, mempty)
    asBindGraph = makeAsBindGraph rhsRef [patAsName]
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

-- | Convert a single function declaration into a SyntaxGraph
translateStringToSyntaxGraph :: String -> SyntaxGraph
translateStringToSyntaxGraph = translateDeclToSyntaxGraph . fromParseResult . parseDecl

syntaxGraphToCollapsedGraph :: SyntaxGraph -> IngSyntaxGraph FGR.Gr
syntaxGraphToCollapsedGraph = collapseNodes . syntaxGraphToFglGraph

translateDeclToCollapsedGraph :: Decl -> IngSyntaxGraph FGR.Gr
translateDeclToCollapsedGraph = syntaxGraphToCollapsedGraph . translateDeclToSyntaxGraph

-- Profiling: about 1.5% of total time.
translateStringToCollapsedGraphAndDecl :: String -> (IngSyntaxGraph FGR.Gr, Decl)
translateStringToCollapsedGraphAndDecl s = (drawing, decl) where
  decl = fromParseResult (parseDecl s) -- :: ParseResult Module
  drawing = translateDeclToCollapsedGraph decl

-- TODO Put the type declarations in a box below the image.
translateModuleToCollapsedGraphs :: Module -> [IngSyntaxGraph FGR.Gr]
translateModuleToCollapsedGraphs (Module _ _ _ _ _ _ decls) = fmap translateDeclToCollapsedGraph decls

-- END Exported functions
