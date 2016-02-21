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

import Types(Icon, Edge(..), Drawing(..), NameAndPort(..), IDState,
  initialIdState, getId)
import Util(toNames, noEnds, nameAndPort, justName, fromMaybeError)
import Icons(Icon(..))

type Reference = Either String NameAndPort
data IconGraph = IconGraph [(DIA.Name, Icon)] [Edge] [(DIA.Name, Drawing)] [(String, NameAndPort)]
  deriving (Show)

type EvalContext = [String]
type GraphAndRef = (IconGraph, Reference)

instance DIA.Semigroup IconGraph where
  (IconGraph icons1 edges1 subDrawings1 context1) <> (IconGraph icons2 edges2 subDrawings2 context2) =
    IconGraph (icons1 <> icons2) (edges1 <> edges2) (subDrawings1 <> subDrawings2) (context1 <> context2)

instance Monoid IconGraph where
  mempty = IconGraph mempty mempty mempty mempty
  mappend = (<>)

getUniqueName :: String -> State IDState String
getUniqueName base = fmap ((base ++). show) getId

nameToString :: Language.Haskell.Exts.Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

evalPattern :: Pat -> String
evalPattern p = case p of
  PVar n -> nameToString n
  -- TODO other cases

evalQName :: QName -> EvalContext -> (IconGraph, Reference)
evalQName (UnQual n) context = result where
  nameString = nameToString n
  graph = IconGraph [(DIA.toName nameString, TextBoxIcon nameString)] mempty mempty mempty
  result = if nameString `elem` context
    then (mempty, Left nameString)
    else (graph, Right $ justName nameString)
-- TODO other cases

evalQOp :: QOp -> EvalContext -> (IconGraph, Reference)
evalQOp (QVarOp n) = evalQName n
evalQOp (QConOp n) = evalQName n

combineExpressions :: [((IconGraph, Reference), NameAndPort)] -> IconGraph
combineExpressions portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  mkGraph ((graph, ref), port) = graph <> case ref of
    Left str -> IconGraph mempty mempty mempty [(str, port)]
    Right resultPort -> IconGraph mempty [Edge (resultPort, port) noEnds] mempty mempty

makeApplyGraph :: DIA.Name -> (IconGraph, Reference) -> [(IconGraph, Reference)] -> Int -> (IconGraph, NameAndPort)
makeApplyGraph applyIconName funVal argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, Apply0NIcon numArgs)]
    newGraph = IconGraph icons mempty mempty mempty

evalApp :: (Exp, [Exp]) -> EvalContext -> State IDState (IconGraph, NameAndPort)
evalApp (funExp, argExps) c = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  pure $ makeApplyGraph applyIconName funVal argVals (length argExps)

evalInfixApp :: EvalContext -> Exp -> QOp -> Exp -> State IDState (IconGraph, NameAndPort)
evalInfixApp c e1 op e2 = do
  argVals <- mapM (evalExp c) [e1, e2]
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  let funVal = evalQOp op c
  pure $ makeApplyGraph applyIconName funVal argVals 2

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
      combineExpressions $ zip [e1Val, e2Val, e3Val] (map (nameAndPort guardName) [3, 2, 4])
    newGraph = IconGraph icons mempty mempty mempty <> combinedGraph
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
    combindedGraph = combineExpressions $ expsWithPorts <> boolsWithPorts
    icons = [(guardName, GuardIcon (length rhss))]
    newGraph = IconGraph icons mempty mempty mempty <> combindedGraph
  pure (newGraph, NameAndPort guardName (Just 0))

makeLiteral :: (Show x) => x -> State IDState (IconGraph, NameAndPort)
makeLiteral x = do
  let str = show x
  name <- DIA.toName <$> getUniqueName str
  let graph = IconGraph [(DIA.toName name, TextBoxIcon str)] mempty mempty mempty
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

-- TODO doing this extra pass here with getBoundVarName suggests that the code should be converted
-- into an intermediate form before conversion to an IconGraph
getBoundVarName :: Decl -> String
getBoundVarName (PatBind _ pat _ _) = evalPattern pat
getBoundVarName (FunBind [Match _ name _ _ _ _]) = nameToString name

--TODO: This needs to add all the extra edges.
evalBinds :: EvalContext -> Binds -> State IDState ([(String, IconGraph)], EvalContext)
evalBinds c (BDecls decls) = do
  let
    boundNames = fmap getBoundVarName decls
    augmentedContext = boundNames <> c
  evaledDecls <- mapM (evalDecl augmentedContext) decls
  pure (zip boundNames evaledDecls, augmentedContext)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

evalLet :: EvalContext -> Binds -> Exp -> State IDState (IconGraph, NameAndPort)
evalLet c bs e = do
  (bindNamesAndGraphs, bindContext) <- evalBinds c bs
  let
    (bindNames, bindGraphs) = unzip bindNamesAndGraphs
    bindGraph = mconcat bindGraphs
  expVal <- coerceExpressionResult <$> evalExp bindContext e
  let (expGraph, expResult) = expVal
  pure $ printSelf (expGraph <> bindGraph, expResult)

evalExp :: EvalContext  -> Exp -> State IDState (IconGraph, Reference)
evalExp c x = case x of
  Var n -> pure $ evalQName n c
  Lit l -> fmap Right <$> evalLit l
  InfixApp e1 op e2 -> fmap Right <$> evalInfixApp c e1 op e2
  e@App{} -> fmap Right <$> evalApp (simplifyApp e) c
  Lambda _ patterns e -> fmap Right <$> evalLambda c patterns e
  Let bs e -> fmap Right <$> evalLet c bs e
  If e1 e2 e3 -> fmap Right <$> evalIf c e1 e2 e3
  Paren e -> evalExp c e
  -- TODO other cases

-- | This is used by the rhs for identity (eg. y x = x)
makeDummyRhs :: String -> (IconGraph, NameAndPort)
makeDummyRhs s = (graph, port) where
  graph = IconGraph icons mempty mempty [(s, justName s)]
  icons = [(DIA.toName s, BranchIcon)]
  port = justName s

coerceExpressionResult :: (IconGraph, Reference) -> (IconGraph, NameAndPort)
coerceExpressionResult (_, Left str) = makeDummyRhs str
coerceExpressionResult (g, Right x) = (g, x)

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: Rhs -> EvalContext -> State IDState (IconGraph, Reference)
evalRhs (UnGuardedRhs e) c = evalExp c e
--  coerceExpressionResult <$> evalExp c e
evalRhs (GuardedRhss rhss) c = fmap Right <$> evalGuardedRhss c rhss
-- TODO implement other cases.
--evalRhs (GuardedRhss _) _ = error "GuardedRhss not implemented"

evalPatBind :: EvalContext -> Decl -> State IDState IconGraph
evalPatBind c (PatBind _ pat rhs _) = do
  let patName = evalPattern pat
  --(rhsGraph, rhsNamePort) <- evalRhs rhs c
  rhsVal <- evalRhs rhs c
  uniquePatName <- getUniqueName patName
  let
    (rhsGraph, rhsRef) = rhsVal
    gr = case rhsRef of
      -- TODO: Add bindings here.
      --(Left str) -> IconGraph mempty mempty mempty [(patName, Left str)]
      (Left _) -> mempty
      (Right rhsNamePort) -> graph
        where
          icons = toNames [(uniquePatName, TextBoxIcon patName)]
          edges = [Edge (justName uniquePatName, rhsNamePort) noEnds]
          graph = if patName `elem` c
            -- todo: add Bindings
            --then IconGraph mempty mempty mempty [(patName, Right rhsNamePort)]
            then mempty
            else IconGraph icons edges mempty mempty
          --pure $ graph <> rhsGraph
  pure (gr <> rhsGraph)

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _) = Drawing icons edges subDrawings

--processPatterns :: DIA.IsName a => a -> [Pat] -> ([(String, NameAndPort)], [String], Int)
processPatterns :: DIA.IsName a  => a -> [Pat] -> [(String, NameAndPort)] -> ([(String, NameAndPort)], [String], Int)
processPatterns lambdaName patterns extraVars =
  (patternStringMap, patternStrings, numParameters)
    where
      lambdaPorts = map (nameAndPort lambdaName) [0,1..]
      patternStringMap = extraVars <> zip (map evalPattern patterns) lambdaPorts
      patternStrings = map fst patternStringMap
      numParameters = length patterns

makeRhsDrawing :: DIA.IsName a => a -> (IconGraph, NameAndPort) -> Drawing
makeRhsDrawing resultIconName (rhsGraph, rhsResult)= rhsDrawing where
  rhsNewIcons = toNames [(resultIconName, ResultIcon)]
  rhsNewEdges = [Edge (rhsResult, justName resultIconName) noEnds]
  rhsGraphWithResult = rhsGraph <> IconGraph rhsNewIcons rhsNewEdges mempty mempty
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
  (IconGraph _ _ _ boundVars) = rhsGraph
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
    drawing = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)] unmatchedBoundVars
  pure (drawing, justName lambdaName)

evalMatch :: EvalContext -> Match -> State IDState IconGraph
evalMatch c (Match _ name patterns _ rhs _) = do
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
      (lambdaName, LambdaRegionIcon numParameters rhsDrawingName),
      (nameString, TextBoxIcon nameString)
      ]
    externalEdges = [Edge (justName nameString, justName lambdaName) noEnds]
    (internalEdges, unmatchedBoundVars) =
      makeInternalEdges lambdaName rhsGraph patternStrings patternStringMap
    drawing = IconGraph icons (externalEdges <> internalEdges)
      [(rhsDrawingName, rhsDrawing)] unmatchedBoundVars
  pure drawing


evalMatches :: EvalContext -> [Match] -> State IDState IconGraph
evalMatches c [] = pure mempty
evalMatches c [match] = evalMatch c match
-- TODO turn more than one match into a case expression.

-- TODO: Use the context in evalPatBind and evalMatches
evalDecl :: EvalContext -> Decl -> State IDState IconGraph
evalDecl c d = evaluatedDecl where
  evaluatedDecl = case d of
    pat@PatBind{} -> evalPatBind c pat
    FunBind matches -> evalMatches c matches
    -- TODO other cases

drawingFromDecl :: Decl -> Drawing
drawingFromDecl d = iconGraphToDrawing $ evalState evaluatedDecl initialIdState
  where evaluatedDecl = evalDecl mempty d

-- Profiling: about 1.5% of time.
translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  drawing = drawingFromDecl decl
