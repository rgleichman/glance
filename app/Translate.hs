{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString,
  drawingFromDecl,
  drawingsFromModule,
  stringToSyntaxGraph
) where

import Diagrams.Prelude((<>))

import Data.Maybe(catMaybes)
import Control.Monad(replicateM)
import Control.Monad.State(State, evalState)
import Data.Either(partitionEithers)
import Data.List(unzip5, unzip4, partition)
import qualified Language.Haskell.Exts as Exts
import Language.Haskell.Exts(Decl(..), parseDecl, Name(..), Pat(..), Rhs(..),
  Exp(..), QName(..), fromParseResult, Match(..), QOp(..), GuardedRhs(..),
  Stmt(..), Binds(..), Alt(..), Module(..), SpecialCon(..))
import qualified Data.Graph.Inductive.PatriciaTree as FGR
--import Data.Maybe(catMaybes)

import GraphAlgorithms(collapseNodes)
import TranslateCore(Reference, SyntaxGraph(..), EvalContext, GraphAndRef, Sink,
  syntaxGraphFromNodes, syntaxGraphFromNodesEdges, getUniqueName, combineExpressions,
  edgesForRefPortList, makeApplyGraph,
  namesInPattern, lookupReference, deleteBindings, makeEdges,
  coerceExpressionResult, makeBox, nTupleString, nListString,
  syntaxGraphToFglGraph, getUniqueString)
import Types(NameAndPort(..), IDState,
  initialIdState, Edge, SyntaxNode(..), IngSyntaxGraph, NodeName, Port(..), SgNamedNode,
  LikeApplyFlavor(..))
import Util(makeSimpleEdge, nameAndPort, justName, mapFst)

-- OVERVIEW --
-- The core functions and data types used in this module are in TranslateCore.
-- The TranslateCore also contains most/all of the translation functions that
-- do not use Language.Haskell.Exts.

-- HELPER FUNCTIONS --

makeVarExp :: String -> Exp
makeVarExp = Var . UnQual . Ident

makeQVarOp :: String -> QOp
makeQVarOp = QVarOp . UnQual . Ident

-- END HELPER FUNCTIONS --

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

evalPApp :: QName -> [Pat] -> State IDState (SyntaxGraph, NameAndPort)
evalPApp name [] = makeBox $ qNameToString name
evalPApp name patterns = do
  patName <- getUniqueName "pat"
  evaledPatterns <- mapM evalPattern patterns
  let
    constructorName = qNameToString name
    gr = makePatternGraph patName constructorName evaledPatterns (length evaledPatterns)
  pure gr

evalPLit :: Exts.Sign -> Exts.Literal -> State IDState (SyntaxGraph, NameAndPort)
evalPLit Exts.Signless l = evalLit l
evalPLit Exts.Negative l = makeBox ('-' : showLiteral l)

evalPAsPat :: Name -> Pat -> State IDState GraphAndRef
evalPAsPat n p = do
  (evaledPatGraph, evaledPatRef) <- evalPattern p
  let
    newBind = [(nameToString n, evaledPatRef)]
    newGraph = SyntaxGraph mempty mempty mempty newBind mempty
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
strToGraphRef :: EvalContext -> String -> State IDState (SyntaxGraph, Reference)
strToGraphRef c str = fmap mapper (makeBox str) where
  mapper gr = if str `elem` c
    then (mempty, Left str)
    else fmap Right gr

evalQName :: QName -> EvalContext -> State IDState (SyntaxGraph, Reference)
evalQName qName@(UnQual _) c = strToGraphRef c (qNameToString qName)
evalQName qName@(Qual _ _) c = strToGraphRef c (qNameToString qName)
evalQName qName _ = fmap Right <$> makeBox (qNameToString qName)

-- evalQOp :: QOp -> EvalContext -> State IDState (SyntaxGraph, Reference)
-- evalQOp (QVarOp n) = evalQName n
-- evalQOp (QConOp n) = evalQName n

-- qOpToString :: QOp -> String
-- qOpToString (QVarOp n) = qNameToString n
-- qOpToString (QConOp n) = qNameToString n

--findReferencedIcon :: Reference -> [(NodeName, Icon)] -> Maybe (Name, Icon)
-- findReferencedIcon :: Either t NameAndPort -> [(NodeName, t1)] -> Maybe (NodeName, t1)
-- findReferencedIcon (Left str) _ = Nothing
-- findReferencedIcon (Right (NameAndPort name _)) nameIconMap = (\x -> (name, x)) <$> lookup name nameIconMap

-- TODO Refactor decideIfNested and makePatternGraph
decideIfNested :: ((SyntaxGraph, t1), t) ->
  (Maybe ((SyntaxGraph, t1), t), Maybe SgNamedNode, [Sink], [(String, Reference)], [(NodeName, NodeName)])
decideIfNested ((SyntaxGraph [nameAndIcon] [] sinks bindings eMap, _), _) = (Nothing, Just nameAndIcon, sinks, bindings, eMap)
decideIfNested valAndPort = (Just valAndPort, Nothing, [], [], [])

-- TODO Consider removing the Int numArgs parameter.
makePatternGraph :: NodeName -> String -> [(SyntaxGraph, Reference)] -> Int -> (SyntaxGraph, NameAndPort)
makePatternGraph applyIconName funStr argVals _ = nestedApplyResult
  where
    argumentPorts = map (nameAndPort applyIconName . Port) [2,3..]
    (unnestedArgsAndPort, nestedArgs, nestedSinks, nestedBindings, nestedEMaps) = unzip5 $ fmap decideIfNested (zip argVals argumentPorts)

    allSinks = mconcat nestedSinks
    allBinds = mconcat nestedBindings

    originalPortExpPairs = catMaybes unnestedArgsAndPort
    portExpressionPairs = originalPortExpPairs
    combinedGraph = combineExpressions True portExpressionPairs
    icons = [(applyIconName, NestedPatternApplyNode funStr nestedArgs)]
    newEMap = ((\(n, _) -> (n, applyIconName))  <$> catMaybes nestedArgs) <> mconcat nestedEMaps
    
    newGraph = SyntaxGraph icons [] allSinks allBinds newEMap
    nestedApplyResult = (newGraph <> combinedGraph, nameAndPort applyIconName (Port 1))

makePatternGraph' :: NodeName -> String -> [(SyntaxGraph, Reference)] -> Int -> (SyntaxGraph, NameAndPort)
makePatternGraph' applyIconName funStr argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName (Port 1))
  where
    argumentPorts = map (nameAndPort applyIconName . Port) [2,3..]
    combinedGraph = combineExpressions True $ zip argVals argumentPorts
    icons = [(applyIconName, PatternApplyNode funStr numArgs)]
    newGraph = syntaxGraphFromNodes icons

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

evalApp :: EvalContext -> LikeApplyFlavor -> (Exp, [Exp]) -> State IDState (SyntaxGraph, NameAndPort)
evalApp c flavor (funExp, argExps) = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- getUniqueName "app0"
  pure $ makeApplyGraph flavor False applyIconName funVal argVals (length argExps)

qOpToExp :: QOp -> Exp
qOpToExp (QVarOp n) = Var n
qOpToExp (QConOp n) = Con n

evalCompose :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalCompose c functions = do
  let reversedFunctios = reverse functions
  evaluatedFunctions <- mapM (evalExp c) reversedFunctios
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  applyIconName <- getUniqueName "compose"
  pure $ makeApplyGraph ComposeNodeFlavor False applyIconName
    (mempty, neverUsedPort) evaluatedFunctions (length evaluatedFunctions)

simplifyCompose :: Exp -> [Exp]
simplifyCompose e = case removeParen e of
  (InfixApp exp1  (QVarOp (UnQual (Symbol "."))) exp2) -> exp1 : simplifyCompose exp2
  x -> [x]

evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState (SyntaxGraph, Reference)
evalInfixApp c e1 (QVarOp (UnQual (Symbol "$"))) e2 = evalExp c (App e1 e2)
evalInfixApp c e1 (QVarOp (UnQual (Symbol "<$>"))) e2 = evalExp c $ App (App (makeVarExp "fmap") e1) e2
evalInfixApp c e1 (QVarOp (UnQual (Symbol "."))) e2 = fmap Right <$> evalCompose c (e1 : simplifyCompose e2)
evalInfixApp c e1 op e2 = evalExp c (App (App (qOpToExp op) e1) e2)

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

removeParen :: Exp -> Exp
removeParen e = case e of
  Paren x -> removeParen x
  x -> x

simplifyExp :: Exp -> Exp
simplifyExp e = case removeParen e of
  InfixApp exp1  (QVarOp (UnQual (Symbol "$"))) exp2 -> App exp1 exp2
  -- Don't convert compose to apply
  InfixApp _  (QVarOp (UnQual (Symbol "."))) _ -> e
  InfixApp exp1 op exp2 -> App (App (qOpToExp op) exp1) exp2
  LeftSection e op -> App (qOpToExp op) e
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

evalIf :: EvalContext -> Exp -> Exp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalIf c e1 e2 e3 = do
  e1Val <- evalExp c e1
  e2Val <- evalExp c e2
  e3Val <- evalExp c e3
  guardName <- getUniqueName "if"
  let
    icons = [(guardName, GuardNode 2)]
    combinedGraph =
      combineExpressions False $ zip [e1Val, e2Val, e3Val] (map (nameAndPort guardName . Port) [3, 2, 4])
    newGraph = syntaxGraphFromNodes icons <> combinedGraph
  pure (newGraph, nameAndPort guardName (Port 0))

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
  guardName <- getUniqueName "guard"
  evaledRhss <- mapM (evalGuaredRhs c) rhss
  let
    (bools, exps) = unzip evaledRhss
    expsWithPorts = zip exps $ map (nameAndPort guardName . Port) [2,4..]
    boolsWithPorts = zip bools $ map (nameAndPort guardName . Port) [3,5..]
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [(guardName, GuardNode (length rhss))]
    newGraph = syntaxGraphFromNodes icons <> combindedGraph
  pure (newGraph, nameAndPort guardName (Port 1))

-- This is in Translate and not Translate core since currently it is only used by evalLit.
makeLiteral :: (Show x) => x -> State IDState (SyntaxGraph, NameAndPort)
makeLiteral = makeBox. show

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
evalBinds :: EvalContext -> Binds -> State IDState (SyntaxGraph, EvalContext)
evalBinds c (BDecls decls) = do
  let
    boundNames = concatMap getBoundVarName decls
    augmentedContext = boundNames <> c
  evaledDecl <- mconcat <$> mapM (evalDecl augmentedContext) decls
  pure (evaledDecl, augmentedContext)

evalGeneralLet :: (EvalContext -> State IDState (SyntaxGraph, Reference)) -> EvalContext -> Binds -> State IDState (SyntaxGraph, Reference)
evalGeneralLet expOrRhsEvaler c bs = do
  (bindGraph, bindContext) <- evalBinds c bs
  expVal <- expOrRhsEvaler bindContext
  let
    (expGraph, expResult) = expVal
    newGraph = deleteBindings . makeEdges $ expGraph <> bindGraph
    bindings = sgSources bindGraph
  pure (newGraph, lookupReference bindings expResult)

evalLet :: EvalContext -> Binds -> Exp -> State IDState (SyntaxGraph, Reference)
evalLet context binds e = evalGeneralLet (`evalExp` e) context binds

-- TODO: Refactor this with evalPatBind
evalPatAndRhs :: EvalContext -> Pat -> Rhs -> Maybe Binds -> State IDState (Bool, SyntaxGraph, Reference, NameAndPort)
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
      length (sgEdges grWithEdges) > (length (sgEdges rhsGraph) + length (sgEdges patGraph))
  pure (patRhsAreConnected, deleteBindings grWithEdges, patRef, rhsRef)

-- returns (combined graph, pattern reference, rhs reference)
evalAlt :: EvalContext -> Exts.Alt -> State IDState (Bool, SyntaxGraph, Reference, NameAndPort)
evalAlt c (Exts.Alt _ pat rhs maybeBinds) = evalPatAndRhs c pat rhs maybeBinds

evalCase :: EvalContext -> Exp -> [Alt] -> State IDState (SyntaxGraph, NameAndPort)
evalCase c e alts = do
  evaledAlts <- mapM (evalAlt c) alts
  (expGraph, expRef) <- evalExp c e
  caseIconName <- getUniqueName "case"
  let
    (patRhsConnected, altGraphs, patRefs, rhsRefs) = unzip4 evaledAlts
    combindedAltGraph = mconcat altGraphs
    numAlts = length alts
    icons = [(caseIconName, CaseNode numAlts)]
    caseGraph = syntaxGraphFromNodes icons
    expEdge = (expRef, nameAndPort caseIconName (Port 0))
    patEdges = zip patRefs $ map (nameAndPort caseIconName . Port) [2,4..]
    rhsEdges = zip patRhsConnected $ zip rhsRefs $ map (nameAndPort caseIconName . Port) [3,5..]
    (connectedRhss, unConnectedRhss) = partition fst rhsEdges
  resultIconNames <- replicateM numAlts (getUniqueName "caseResult")
  let
    makeCaseResult resultIconName rhsPort = syntaxGraphFromNodesEdges rhsNewIcons rhsNewEdges
      where
        rhsNewIcons = [(resultIconName, CaseResultNode)]
        rhsNewEdges = [makeSimpleEdge (rhsPort, justName resultIconName)]
    caseResultGraphs = mconcat $ zipWith makeCaseResult resultIconNames (fmap (fst . snd) connectedRhss)
    filteredRhsEdges = mapFst Right $ fmap snd unConnectedRhss
    patternEdgesGraph = edgesForRefPortList True patEdges
    caseEdgeGraph = edgesForRefPortList False (expEdge : filteredRhsEdges)
    finalGraph = mconcat [patternEdgesGraph, caseResultGraphs, expGraph, caseEdgeGraph, caseGraph, combindedAltGraph]
  pure (finalGraph, nameAndPort caseIconName (Port 1))

evalTuple :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalTuple c exps = do
  argVals <- mapM (evalExp c) exps
  funVal <- makeBox $ nTupleString (length exps)
  applyIconName <- getUniqueName "tupleApp"
  pure $ makeApplyGraph ApplyNodeFlavor False applyIconName (fmap Right funVal) argVals (length exps)

evalListExp :: EvalContext -> [Exp] -> State IDState (SyntaxGraph, NameAndPort)
evalListExp _ [] = makeBox "[]"
evalListExp c exps = evalApp c ApplyNodeFlavor (makeVarExp . nListString . length $ exps, exps)

evalLeftSection :: EvalContext -> Exp -> QOp -> State IDState (SyntaxGraph, Reference)
evalLeftSection c e op = evalExp c $ App (qOpToExp op) e

evalRightSection :: EvalContext -> QOp -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalRightSection c op e = do
  expVal <- evalExp c e
  funVal <- evalExp c (qOpToExp op)
  applyIconName <- getUniqueName "tupleApp"
  -- TODO: A better option would be for makeApplyGraph to take the list of expressions as Maybes.
  neverUsedPort <- Left <$> getUniqueString "unusedArgument"
  pure $ makeApplyGraph ApplyNodeFlavor False applyIconName funVal [(mempty, neverUsedPort), expVal] 2

-- evalEnums is only used by evalExp
evalEnums :: EvalContext -> String -> [Exp] -> State IDState (SyntaxGraph, Reference)
evalEnums c s exps = fmap Right <$> evalApp c ApplyNodeFlavor (makeVarExp s, exps)

desugarDo :: [Stmt] -> Exp
desugarDo [Qualifier e] = e
desugarDo (Qualifier e : stmts) = InfixApp e thenOp (desugarDo stmts)
  where thenOp = makeQVarOp ">>"
desugarDo (Generator srcLoc pat e : stmts) =
  InfixApp e  (makeQVarOp ">>=") (Lambda srcLoc [pat] (desugarDo stmts))
desugarDo (LetStmt binds : stmts) = Let binds (desugarDo stmts)

-- TODO: Finish evalRecConstr
evalRecConstr :: EvalContext -> QName -> [Exts.FieldUpdate] -> State IDState (SyntaxGraph, Reference)
evalRecConstr c qName _ = evalQName qName c

evalExp :: EvalContext  -> Exp -> State IDState (SyntaxGraph, Reference)
evalExp c x = case x of
  Var n -> evalQName n c
  Con n -> evalQName n c
  Lit l -> fmap Right <$> evalLit l
  InfixApp e1 op e2 -> evalInfixApp c e1 op e2
  App f arg -> fmap Right <$> evaluateAppExpression c f arg
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
  LeftSection e op -> evalLeftSection c e op
  RightSection op e -> fmap Right <$> evalRightSection c op e
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

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: EvalContext -> Rhs -> State IDState (SyntaxGraph, Reference)
evalRhs c (UnGuardedRhs e) = evalExp c e
evalRhs c (GuardedRhss rhss) = fmap Right <$> evalGuardedRhss c rhss

rhsWithBinds :: Maybe Binds -> Rhs -> EvalContext -> State IDState (SyntaxGraph, Reference)
rhsWithBinds maybeWhereBinds rhs rhsContext = case maybeWhereBinds of
  Nothing -> evalRhs rhsContext rhs
  Just b -> evalGeneralLet (`evalRhs` rhs) rhsContext b

evalPatBind :: EvalContext -> Decl -> State IDState SyntaxGraph
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
    gr = SyntaxGraph mempty newEdges newSinks bindings mempty
  pure . makeEdges $ (gr <> rhsGraph <> patGraph)

generalEvalLambda :: EvalContext -> [Pat] -> (EvalContext -> State IDState GraphAndRef) -> State IDState (SyntaxGraph, NameAndPort)
generalEvalLambda context patterns rhsEvalFun = do
  lambdaName <- getUniqueName "lam"
  patternVals <- mapM evalPattern patterns
  let
    patternStrings = concatMap namesInPattern patternVals
    rhsContext = patternStrings <> context
    lambdaPorts = map (nameAndPort lambdaName . Port) [2,3..]
    patternGraph = mconcat $ map fst patternVals

    (patternEdges, newBinds) =
      partitionEithers $ zipWith makePatternEdges patternVals lambdaPorts
    numParameters = length patterns
  -- TODO remove coerceExpressionResult here
  (rhsRawGraph, rhsResult) <- rhsEvalFun rhsContext >>= coerceExpressionResult
  let
    icons = [(lambdaName, FunctionDefNode numParameters)]
    resultIconEdge = makeSimpleEdge (rhsResult, nameAndPort lambdaName (Port 0))
    finalGraph = SyntaxGraph icons (resultIconEdge:patternEdges)
      mempty newBinds mempty
  pure (deleteBindings . makeEdges $ (rhsRawGraph <> patternGraph <> finalGraph), nameAndPort lambdaName (Port 1))
  where
    -- TODO Like evalPatBind, this edge should have an indicator that it is the input to a pattern.
    -- makePatternEdges creates the edges between the patterns and the parameter ports.
    makePatternEdges :: GraphAndRef -> NameAndPort -> Either Edge (String, Reference)
    makePatternEdges (_, Right patPort) lamPort =
      Left $ makeSimpleEdge (lamPort, patPort)
    makePatternEdges (_, Left str) lamPort = Right (str, Right lamPort)


evalLambda :: EvalContext -> [Pat] -> Exp -> State IDState (SyntaxGraph, NameAndPort)
evalLambda c patterns e = generalEvalLambda c patterns (`evalExp` e)

evalMatch :: EvalContext -> Match -> State IDState SyntaxGraph
evalMatch c (Match _ name patterns _ rhs maybeWhereBinds) = do
  let
    matchFunNameString = nameToString name
    newContext = matchFunNameString : c
  (lambdaGraph, lambdaPort) <-
    generalEvalLambda newContext patterns (rhsWithBinds maybeWhereBinds rhs)
  let
    newBinding = SyntaxGraph mempty mempty mempty [(matchFunNameString, Right lambdaPort)] mempty
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


evalMatches :: EvalContext -> [Match] -> State IDState SyntaxGraph
evalMatches _ [] = pure mempty
evalMatches c (firstMatch:restOfMatches) = matchesToCase firstMatch restOfMatches >>= evalMatch c

evalDecl :: EvalContext -> Decl -> State IDState SyntaxGraph
evalDecl c d = evaluatedDecl where
  evaluatedDecl = case d of
    pat@(PatBind _ _ _ _) -> evalPatBind c pat
    FunBind matches -> evalMatches c matches
    --TODO: Add other cases here
    _ -> pure mempty

showTopLevelBinds :: SyntaxGraph -> State IDState SyntaxGraph
showTopLevelBinds gr = do
  let
    binds = sgSources gr
    addBind (_, Left _) = pure mempty
    addBind (patName, Right port) = do
      uniquePatName <- getUniqueName patName
      let
        icons = [(uniquePatName, BindNameNode patName)]
        edges = [makeSimpleEdge (port, justName uniquePatName)]
        edgeGraph = syntaxGraphFromNodesEdges icons edges
      pure edgeGraph
  newGraph <- mconcat <$> mapM addBind binds
  pure $ newGraph <> gr

-- TODO Rename these functions to not have "drawing" in them.
drawingFromDecl :: Decl -> IngSyntaxGraph FGR.Gr
drawingFromDecl d = drawing
  where
    evaluatedDecl = evalDecl mempty d >>= showTopLevelBinds
    syntaxGraph = evalState evaluatedDecl initialIdState
    drawing = collapseNodes $ syntaxGraphToFglGraph syntaxGraph
    --drawing = syntaxGraphToFglGraph syntaxGraph

-- Profiling: about 1.5% of total time.
translateString :: String -> (IngSyntaxGraph FGR.Gr, Decl)
translateString s = (drawing, decl) where
  decl = fromParseResult (parseDecl s) -- :: ParseResult Module
  drawing = drawingFromDecl decl

drawingsFromModule :: Module -> [IngSyntaxGraph FGR.Gr]
drawingsFromModule (Module _ _ _ _ _ _ decls) = fmap drawingFromDecl decls

stringToSyntaxGraph :: String -> SyntaxGraph
stringToSyntaxGraph s = graph where
  decl = fromParseResult (parseDecl s)
  evaluatedDecl = evalDecl mempty decl >>= showTopLevelBinds
  graph = evalState evaluatedDecl initialIdState
