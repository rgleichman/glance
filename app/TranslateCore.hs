module TranslateCore(
  Reference,
  SyntaxGraph(..),
  EvalContext,
  GraphAndRef,
  Sink,
  syntaxGraphFromNodes,
  syntaxGraphFromNodesEdges,
  getUniqueName,
  edgesForRefPortList,
  combineExpressions,
  --qualifyNameAndPort,
  makeApplyGraph,
  namesInPattern,
  lookupReference,
  deleteBindings,
  makeEdges,
  --makeEdgesCore,
  coerceExpressionResult,
  makeBox,
  nTupleString,
  nListString,
  syntaxGraphToDrawing,
  syntaxGraphToFglGraph,
  ingSyntaxGraphToDrawing,
  nodeToIcon
) where

import Data.Semigroup(Semigroup, (<>))
import qualified Diagrams.Prelude as DIA
import Control.Monad.State(State)
import Data.Either(partitionEithers)
import Control.Arrow(second)
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive as ING
import Diagrams.TwoD.GraphViz as DiaGV
import Data.List(find)

import Types(Icon, SyntaxNode(..), Edge(..), EdgeOption(..), Drawing(..),
  NameAndPort(..), IDState, getId, SgNamedNode, IngSyntaxGraph)
import Util(noEnds, nameAndPort, makeSimpleEdge, justName, fromMaybeError, maybeBoolToBool)
import Icons(Icon(..))

-- OVERVIEW --
-- This module has the core functions and data types used by Translate.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.
-- * Please note that type DIA.Name is not the Name from Language.Haskell.Exts
-- used in Translate.

type Reference = Either String NameAndPort

-- | SyntaxGraph is an abstract representation for Haskell syntax. SyntaxGraphs are
-- generated from the Haskell syntax tree, and are used to generate Drawings
data SyntaxGraph = SyntaxGraph {
  sgNodes :: [SgNamedNode],
  sgEdges :: [Edge],
  sgSinks :: [(String, NameAndPort)],
  sgSources :: [(String, Reference)]
  } deriving (Show)

instance Semigroup SyntaxGraph where
  (SyntaxGraph icons1 edges1 sinks1 sources1) <> (SyntaxGraph icons2 edges2 sinks2 sources2) =
    SyntaxGraph (icons1 <> icons2) (edges1 <> edges2) (sinks1 <> sinks2) (sources1 <> sources2)

instance Monoid SyntaxGraph where
  mempty = SyntaxGraph mempty mempty mempty mempty
  mappend = (<>)

type EvalContext = [String]
type GraphAndRef = (SyntaxGraph, Reference)
type Sink = (String, NameAndPort)

syntaxGraphFromNodes :: [(DIA.Name, SyntaxNode)] -> SyntaxGraph
syntaxGraphFromNodes icons = SyntaxGraph icons mempty mempty mempty

syntaxGraphFromNodesEdges :: [(DIA.Name, SyntaxNode)] -> [Edge] -> SyntaxGraph
syntaxGraphFromNodesEdges icons edges = SyntaxGraph icons edges mempty mempty

getUniqueName :: String -> State IDState String
getUniqueName base = fmap ((base ++). show) getId

-- TODO: Refactor with combineExpressions
edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> SyntaxGraph
edgesForRefPortList inPattern portExpPairs = mconcat $ fmap makeGraph portExpPairs where
  edgeOpts = if inPattern then [EdgeInPattern] else []
  makeGraph (ref, port) = case ref of
    Left str -> if inPattern
      then SyntaxGraph mempty mempty mempty [(str, Right port)]
      else SyntaxGraph mempty mempty [(str, port)] mempty
    Right resultPort -> SyntaxGraph mempty [Edge edgeOpts noEnds (resultPort, port)] mempty mempty

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> SyntaxGraph
combineExpressions inPattern portExpPairs = mconcat $ fmap makeGraph portExpPairs where
  edgeOpts = if inPattern then [EdgeInPattern] else []
  makeGraph ((graph, ref), port) = graph <> case ref of
    Left str -> if inPattern
      then SyntaxGraph mempty mempty mempty [(str, Right port)]
      else SyntaxGraph mempty mempty [(str, port)] mempty
    Right resultPort -> SyntaxGraph mempty [Edge edgeOpts noEnds (resultPort, port)] mempty mempty

-- qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
-- qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

makeApplyGraph :: Bool -> DIA.Name -> GraphAndRef -> [GraphAndRef] -> Int -> (SyntaxGraph, NameAndPort)
makeApplyGraph inPattern applyIconName funVal argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions inPattern $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, ApplyNode numArgs)]
    newGraph = syntaxGraphFromNodes icons

namesInPattern :: GraphAndRef -> [String]
namesInPattern (_, Left str) = [str]
namesInPattern (SyntaxGraph _ _ _ bindings, Right _) = fmap fst bindings

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

deleteBindings :: SyntaxGraph -> SyntaxGraph
deleteBindings (SyntaxGraph a b c _) = SyntaxGraph a b c mempty

makeEdgesCore :: [Sink] -> [(String, Reference)] -> ([Sink], [Edge])
makeEdgesCore sinks bindings = partitionEithers $ fmap renameOrMakeEdge sinks
  where
    renameOrMakeEdge :: (String, NameAndPort) -> Either (String, NameAndPort) Edge
    renameOrMakeEdge orig@(s, destPort) = case lookup s bindings of
      Just ref -> case lookupReference bindings ref of
        (Right sourcePort) -> Right $ makeSimpleEdge (sourcePort, destPort)
        (Left newStr) -> Left (newStr, destPort)
      Nothing -> Left orig

makeEdges :: SyntaxGraph -> SyntaxGraph
makeEdges (SyntaxGraph icons edges sinks bindings) = newGraph where
  (newSinks, newEdges) = makeEdgesCore sinks bindings
  newGraph = SyntaxGraph icons (newEdges <> edges) newSinks bindings

-- TODO: Remove BranchNode
-- | This is used by the rhs for identity (eg. y x = x)
coerceExpressionResult :: (SyntaxGraph, Reference) -> State IDState (SyntaxGraph, NameAndPort)
coerceExpressionResult (_, Left str) = makeDummyRhs str where
  makeDummyRhs :: String -> State IDState (SyntaxGraph, NameAndPort)
  makeDummyRhs s = do
    iconName <- getUniqueName s
    let
      graph = SyntaxGraph icons mempty [(s, port)] mempty
      icons = [(DIA.toName iconName, BranchNode)]
      port = justName iconName
    pure (graph, port)
coerceExpressionResult (g, Right x) = pure (g, x)

-- TODO: remove / change due toSyntaxGraph
makeBox :: String -> State IDState (SyntaxGraph, NameAndPort)
makeBox str = do
  name <- DIA.toName <$> getUniqueName str
  let graph = syntaxGraphFromNodes [(DIA.toName name, LiteralNode str)]
  pure (graph, justName name)

nTupleString :: Int -> String
nTupleString n = '(' : replicate (n -1) ',' ++ ")"

nListString :: Int -> String
-- TODO: Use something better than [_]
nListString 1 = "[_]"
nListString n = '[' : replicate (n -1) ',' ++ "]"

nodeToIcon :: SyntaxNode -> Icon
nodeToIcon (ApplyNode n) = ApplyAIcon n
nodeToIcon (NestedApplyNode x edges) = nestedApplySyntaxNodeToIcon x edges
nodeToIcon (PatternApplyNode s n) = PAppIcon n s
nodeToIcon (NestedPatternApplyNode s n children) = nestedPatternNodeToIcon s n children
nodeToIcon (NameNode s) = TextBoxIcon s
nodeToIcon (BindNameNode s) = BindTextBoxIcon s
nodeToIcon (LiteralNode s) = TextBoxIcon s
nodeToIcon (FunctionDefNode n) = FlatLambdaIcon n
nodeToIcon (GuardNode n) = GuardIcon n
nodeToIcon (CaseNode n) = CaseIcon n
nodeToIcon BranchNode = BranchIcon
nodeToIcon CaseResultNode = CaseResultIcon

makeArg :: [(SgNamedNode, Edge)] -> Int -> Maybe (DIA.Name, Icon)
makeArg args port = case find (findArg port) args of
  Nothing -> Nothing
  Just ((argName, argSyntaxNode), _) -> Just (argName, nodeToIcon argSyntaxNode)

nestedApplySyntaxNodeToIcon :: Int -> [(SgNamedNode, Edge)] -> Icon
nestedApplySyntaxNodeToIcon numArgs args = NestedApply argList where
  -- argList should be of length numArgs + 1, since argList includes the function expression
  -- port 0 is the function, ports 2..(numArgs+1) are the arguments
  -- TODO Don't use hardcoded port numbers
  argList = fmap (makeArg args) (0:[2..numArgs + 1])

nestedPatternNodeToIcon :: String -> Int -> [(SgNamedNode, Edge)] -> Icon
nestedPatternNodeToIcon str numArgs args = NestedPApp argList where
  -- TODO Using [toName ""] is probably not the best thing to do.
  -- TODO Don't use hardcoded port numbers
  argList = Just (DIA.toName "", TextBoxIcon str) : fmap (makeArg args) [2..numArgs + 1]

findArg :: Int -> (SgNamedNode, Edge) -> Bool
findArg currentPort ((argName, _), Edge _ _ (NameAndPort fromName fromPort, NameAndPort toName toPort))
  | argName == fromName = maybeBoolToBool $ fmap (== currentPort) toPort
  | argName == toName = maybeBoolToBool $ fmap (== currentPort) fromPort
  | otherwise = False -- This case should never happen

syntaxGraphToFglGraph :: SyntaxGraph -> FGR.Gr SgNamedNode Edge
syntaxGraphToFglGraph (SyntaxGraph nodes edges _ _) =
  DiaGV.mkGraph nodes labeledEdges where
    labeledEdges = fmap makeLabeledEdge edges
    makeLabeledEdge e@(Edge _ _ (NameAndPort name1 _, NameAndPort name2 _)) =
      ((name1, lookupInNodes name1), (name2, lookupInNodes name2), e) where
        lookupInNodes name = fromMaybeError errorString (lookup name nodes) where
          errorString =
            "syntaxGraphToFglGraph edge connects to non-existent node. Node Name ="
            ++ show name ++ " Edge=" ++ show e


syntaxGraphToDrawing :: SyntaxGraph -> Drawing
syntaxGraphToDrawing (SyntaxGraph nodes edges _ _) =
  Drawing icons edges where
    icons = fmap (second nodeToIcon) nodes

ingSyntaxGraphToDrawing :: ING.Graph gr => IngSyntaxGraph gr -> Drawing
ingSyntaxGraphToDrawing syntaxGraph = Drawing icons edges where
  icons = (second nodeToIcon . snd) <$> ING.labNodes syntaxGraph
  edges = ING.edgeLabel <$> ING.labEdges syntaxGraph
