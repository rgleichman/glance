{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude((<>))

import Language.Haskell.Exts(Decl(..), parseDecl,
  Name(..), Pat(..), Rhs(..), Exp(..), QName(..), fromParseResult, Match(..)) --(parseFile, parse, ParseResult, Module)
import Control.Monad.State(State, evalState)
import Data.List(partition)
import qualified Control.Arrow

import Types(Icon, Edge(..), Drawing(..), NameAndPort(..), IDState,
  initialIdState, getId)
import Util(toNames, noEnds, nameAndPort, justName, fromMaybeError)
import Icons(Icon(..))

data IconGraph = IconGraph [(DIA.Name, Icon)] [Edge] [(DIA.Name, Drawing)] [(String, NameAndPort)]

type EvalContext = [String]

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

evalQName :: QName -> EvalContext -> Either String (IconGraph, NameAndPort)
evalQName (UnQual n) context = result where
  nameString = nameToString n
  graph = IconGraph [(DIA.toName nameString, TextBoxIcon nameString)] mempty mempty mempty
  result = if nameString `elem` context
    then Left nameString
    else Right (graph, justName nameString)
-- TODO other cases

combineExpressions :: [(Either String (IconGraph, NameAndPort), NameAndPort)] -> IconGraph
combineExpressions portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  mkGraph portExpPair@(e, port) = case e of
    Left str -> IconGraph mempty mempty mempty [(str, port)]
    Right (graph, resultPort) -> newGraph <> graph where
      newGraph = IconGraph mempty [Edge (resultPort, port) noEnds] mempty mempty

evalApp :: (Exp, [Exp]) -> EvalContext -> State IDState (IconGraph, NameAndPort)
evalApp (funExp, argExps) c = do
  funVal <- evalExp c funExp
  argVals <- mapM (evalExp c) argExps
  applyIconName <- DIA.toName <$> getUniqueName "app0"
  let
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, Apply0NIcon (length argExps))]
    newGraph = IconGraph icons mempty mempty mempty
  pure (newGraph <> combinedGraph, nameAndPort applyIconName 1)

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

evalExp :: EvalContext  -> Exp -> State IDState (Either String (IconGraph, NameAndPort))
evalExp c x = case x of
  Var n -> pure $ evalQName n c
  e@App{} -> Right <$> evalApp (simplifyApp e) c
  Paren e -> evalExp c e
  Lambda _ patterns e -> Right <$> evalLambda c patterns e
  If e1 e2 e3 -> Right <$> evalIf c e1 e2 e3
  -- TODO other cases

-- | This is used by the rhs for identity (eg. y x = x)
makeDummyRhs :: String -> (IconGraph, NameAndPort)
makeDummyRhs s = (graph, port) where
  graph = IconGraph icons mempty mempty [(s, justName s)]
  icons = [(DIA.toName s, BranchIcon)]
  port = justName s

coerceExpressionResult :: Either String (IconGraph, NameAndPort) -> (IconGraph, NameAndPort)
coerceExpressionResult (Left str) = makeDummyRhs str
coerceExpressionResult (Right x) = x

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: Rhs -> EvalContext -> State IDState (IconGraph, NameAndPort)
evalRhs (UnGuardedRhs e) scope =
  coerceExpressionResult <$> evalExp scope e
-- TODO implement other cases.
--evalRhs (GuardedRhss _) _ = error "GuardedRhss not implemented"

evalPatBind :: Decl -> State IDState IconGraph
evalPatBind (PatBind _ pat rhs _) = do
  let patName = evalPattern pat
  (rhsGraph, rhsNamePort) <- evalRhs rhs []
  uniquePatName <- getUniqueName patName
  let
    icons = toNames [(uniquePatName, TextBoxIcon patName)]
    edges = [Edge (justName uniquePatName, rhsNamePort) noEnds]
    graph = IconGraph icons edges mempty mempty
  pure $ graph <> rhsGraph

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
    rhsCoercedVal@(rhsGraph, _) = coerceExpressionResult rhsVal
    rhsDrawing = makeRhsDrawing resultIconName rhsCoercedVal
    icons = toNames [(lambdaName, LambdaRegionIcon numParameters rhsDrawingName)]
    (internalEdges, unmatchedBoundVars) =
      makeInternalEdges lambdaName rhsGraph patternStrings patternStringMap
    drawing = IconGraph icons internalEdges [(rhsDrawingName, rhsDrawing)] unmatchedBoundVars
  pure (drawing, justName lambdaName)

evalMatch :: Match -> State IDState IconGraph
evalMatch (Match _ name patterns _ rhs _) = do
  lambdaName <- getUniqueName "lam"
  let
    nameString = nameToString name
    extraVars = [(nameString, justName lambdaName)]
    (patternStringMap, patternStrings, numParameters) =
      processPatterns lambdaName patterns extraVars

  rhsVal@(rhsGraph, _) <- evalRhs rhs patternStrings
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


evalMatches :: [Match] -> State IDState IconGraph
evalMatches [] = pure mempty
evalMatches [match] = evalMatch match
-- TODO turn more than one match into a case expression.

evalDecl :: Decl -> Drawing
evalDecl d = iconGraphToDrawing $ evalState evaluatedDecl initialIdState where
  evaluatedDecl = case d of
    pat@PatBind{} -> evalPatBind pat
    FunBind matches -> evalMatches matches
    -- TODO other cases

translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  drawing = evalDecl decl
