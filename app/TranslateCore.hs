module TranslateCore(
  Reference,
  IconGraph(..),
  EvalContext,
  GraphAndRef,
  Sink,
  iconGraphFromIcons,
  iconGraphFromIconsEdges,
  getUniqueName,
  edgesForRefPortList,
  combineExpressions,
  --qualifyNameAndPort,
  iconGraphToDrawing,
  makeApplyGraph,
  namesInPattern,
  lookupReference,
  deleteBindings,
  makeEdges,
  --makeEdgesCore,
  coerceExpressionResult,
  makeBox,
  nTupleString,
  nListString
) where

import Data.Semigroup(Semigroup, (<>))
import qualified Diagrams.Prelude as DIA
import Control.Monad.State(State)
import Data.Either(partitionEithers)

import Types(Icon, Edge(..), EdgeOption(..), Drawing(..), NameAndPort(..), IDState,
  getId)
import Util(noEnds, nameAndPort, makeSimpleEdge, justName)
import Icons(Icon(..))

-- OVERVIEW --
-- This module has the core functions and data types used by Translate.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.
-- * Please note that type DIA.Name is not the Name from Language.Haskell.Exts
-- used in Translate.

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

instance Semigroup IconGraph where
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

-- TODO: Refactor with combineExpressions
edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> IconGraph
edgesForRefPortList inPattern portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  edgeOpts = if inPattern then [EdgeInPattern] else []
  mkGraph (ref, port) = case ref of
    Left str -> if inPattern
      then IconGraph mempty mempty mempty mempty [(str, Right port)]
      else IconGraph mempty mempty mempty [(str, port)] mempty
    Right resultPort -> IconGraph mempty [Edge edgeOpts noEnds (resultPort, port)] mempty mempty mempty

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> IconGraph
combineExpressions inPattern portExpPairs = mconcat $ fmap mkGraph portExpPairs where
  edgeOpts = if inPattern then [EdgeInPattern] else []
  mkGraph ((graph, ref), port) = graph <> case ref of
    Left str -> if inPattern
      then IconGraph mempty mempty mempty mempty [(str, Right port)]
      else IconGraph mempty mempty mempty [(str, port)] mempty
    Right resultPort -> IconGraph mempty [Edge edgeOpts noEnds (resultPort, port)] mempty mempty mempty

-- qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
-- qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _ _) = Drawing icons edges subDrawings

makeApplyGraph :: Bool -> DIA.Name -> GraphAndRef -> [GraphAndRef] -> Int -> (IconGraph, NameAndPort)
makeApplyGraph inPattern applyIconName funVal argVals numArgs = (newGraph <> combinedGraph, nameAndPort applyIconName 1)
  where
    argumentPorts = map (nameAndPort applyIconName) [2,3..]
    functionPort = nameAndPort applyIconName 0
    combinedGraph = combineExpressions inPattern $ zip (funVal:argVals) (functionPort:argumentPorts)
    icons = [(applyIconName, ApplyAIcon numArgs)]
    newGraph = iconGraphFromIcons icons

namesInPattern :: GraphAndRef -> [String]
namesInPattern (_, Left str) = [str]
namesInPattern (IconGraph _ _ _ _ bindings, Right _) = fmap fst bindings

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
        (Right sourcePort) -> Right $ makeSimpleEdge (sourcePort, destPort)
        (Left newStr) -> Left (newStr, destPort)
      Nothing -> Left orig

makeEdges :: IconGraph -> IconGraph
makeEdges (IconGraph icons edges c sinks bindings) = newGraph where
  (newSinks, newEdges) = makeEdgesCore sinks bindings
  newGraph = IconGraph icons (newEdges <> edges) c newSinks bindings

-- | This is used by the rhs for identity (eg. y x = x)
coerceExpressionResult :: (IconGraph, Reference) -> State IDState (IconGraph, NameAndPort)
coerceExpressionResult (_, Left str) = makeDummyRhs str where
  makeDummyRhs :: String -> State IDState (IconGraph, NameAndPort)
  makeDummyRhs s = do
    iconName <- getUniqueName s
    let
      graph = IconGraph icons mempty mempty [(s, port)] mempty
      icons = [(DIA.toName iconName, BranchIcon)]
      port = justName iconName
    pure (graph, port)
coerceExpressionResult (g, Right x) = pure (g, x)

makeBox :: String -> State IDState (IconGraph, NameAndPort)
makeBox str = do
  name <- DIA.toName <$> getUniqueName str
  let graph = iconGraphFromIcons [(DIA.toName name, TextBoxIcon str)]
  pure (graph, justName name)

nTupleString :: Int -> String
nTupleString n = '(' : replicate (n -1) ',' ++ ")"

nListString :: Int -> String
-- TODO: Use something better than [_]
nListString 1 = "[_]"
nListString n = '[' : replicate (n -1) ',' ++ "]"
