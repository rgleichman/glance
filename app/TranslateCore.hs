-- This file is formatted with Ormolu
module TranslateCore
  ( Reference,
    SyntaxGraph (..),
    EvalContext,
    GraphAndRef (..),
    SgSink (..),
    SgBind (..),
    syntaxGraphFromNodes,
    syntaxGraphFromNodesEdges,
    bindsToSyntaxGraph,
    graphAndRefToGraph,
    getUniqueName,
    getUniqueString,
    edgesForRefPortList,
    combineExpressions,
    makeApplyGraph,
    makeMultiIfGraph,
    namesInPattern,
    lookupReference,
    deleteBindings,
    makeEdges,
    makeBox,
    nTupleString,
    nTupleSectionString,
    nListString,
    syntaxGraphToFglGraph,
    nodeToIcon,
    initialIdState,
  )
where

import Control.Monad.State (State, state)
import Data.Either (partitionEithers)
import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Icons
  ( argumentPorts,
    inputPort,
    multiIfBoolPorts,
    multiIfRhsPorts,
    resultPort,
  )
import Types
  ( CaseOrMultiIfTag (..),
    Edge (..),
    EdgeOption (..),
    Embedder (..),
    EmbedderSyntaxNode,
    IDState (..),
    Icon (..),
    Labeled (..),
    LikeApplyFlavor (..),
    NameAndPort (..),
    Named (..),
    NodeName (..),
    Port,
    SgNamedNode,
    SyntaxNode (..),
    mkEmbedder,
  )
import Util
  ( justName,
    makeSimpleEdge,
    maybeBoolToBool,
    nameAndPort,
    nodeNameToInt,
  )

{-# ANN module "HLint: ignore Use list comprehension" #-}

-- OVERVIEW --
-- This module has the core functions and data types used by Translate.
-- This module also contains most/all of the translation functions that
-- do not require Language.Haskell.Exts.

type Reference = Either String NameAndPort

type EvalContext = [String]

data SgBind = SgBind String Reference deriving (Eq, Show, Ord)

data SgSink = SgSink String NameAndPort deriving (Eq, Ord, Show)

-- TODO Replace lists with sets

-- | A SyntaxGraph is an abstract representation of Haskell syntax. SyntaxGraphs
-- are generated from the Haskell syntax tree and are used to generate Drawings.
data SyntaxGraph = SyntaxGraph
  { sgNodes :: [SgNamedNode],
    sgEdges :: [Edge],
    sgSinks :: [SgSink],
    sgBinds :: [SgBind],
    -- sgEmbedMap keeps track of nodes embedded in other nodes. If (child, parent)
    -- is in the Map, then child is embedded inside parent.
    sgEmbedMap :: Map.Map NodeName NodeName
  }
  deriving (Show, Eq)

instance Semigroup SyntaxGraph where
  (<>)
    (SyntaxGraph icons1 edges1 sinks1 sources1 map1)
    (SyntaxGraph icons2 edges2 sinks2 sources2 map2) =
      SyntaxGraph
        (icons1 <> icons2)
        (edges1 <> edges2)
        (sinks1 <> sinks2)
        (sources1 <> sources2)
        (map1 <> map2)

instance Monoid SyntaxGraph where
  mempty = SyntaxGraph mempty mempty mempty mempty mempty
  mappend = (<>)

data GraphAndRef = GraphAndRef SyntaxGraph Reference

-- BEGIN Constructors and Destructors

sgBindToString :: SgBind -> String
sgBindToString (SgBind s _) = s

sgBindToTuple :: SgBind -> (String, Reference)
sgBindToTuple (SgBind s r) = (s, r)

syntaxGraphFromNodes :: [SgNamedNode] -> SyntaxGraph
syntaxGraphFromNodes icons = SyntaxGraph icons mempty mempty mempty mempty

syntaxGraphFromNodesEdges :: [SgNamedNode] -> [Edge] -> SyntaxGraph
syntaxGraphFromNodesEdges icons edges =
  SyntaxGraph icons edges mempty mempty mempty

bindsToSyntaxGraph :: [SgBind] -> SyntaxGraph
bindsToSyntaxGraph binds = SyntaxGraph mempty mempty mempty binds mempty

sinksToSyntaxGraph :: [SgSink] -> SyntaxGraph
sinksToSyntaxGraph sinks = SyntaxGraph mempty mempty sinks mempty mempty

edgesToSyntaxGraph :: [Edge] -> SyntaxGraph
edgesToSyntaxGraph edges = SyntaxGraph mempty edges mempty mempty mempty

graphAndRefToGraph :: GraphAndRef -> SyntaxGraph
graphAndRefToGraph (GraphAndRef g _) = g

-- END Constructors and Destructors

-- BEGIN IDState

initialIdState :: IDState
initialIdState = IDState 0

getId :: State IDState Int
getId = state incrementer
  where
    incrementer (IDState x) = (x, IDState checkedIncrement)
      where
        xPlusOne = x + 1
        checkedIncrement =
          if xPlusOne > x
            then xPlusOne
            else error "getId: the ID state has overflowed."

getUniqueName :: State IDState NodeName
getUniqueName = fmap NodeName getId

-- TODO Should getUniqueString prepend an illegal character?
getUniqueString :: String -> State IDState String
getUniqueString base = fmap ((base ++) . show) getId

-- END IDState

-- TODO: Refactor with combineExpressions
edgesForRefPortList :: Bool -> [(Reference, NameAndPort)] -> SyntaxGraph
edgesForRefPortList inPattern portExpPairs =
  mconcat $ fmap makeGraph portExpPairs
  where
    edgeOpts = if inPattern then [EdgeInPattern] else []
    makeGraph (ref, port) = case ref of
      Left str ->
        if inPattern
          then bindsToSyntaxGraph [SgBind str (Right port)]
          else sinksToSyntaxGraph [SgSink str port]
      Right resPort -> edgesToSyntaxGraph [Edge edgeOpts connection]
        where
          connection =
            if inPattern
              then -- If in a pattern, then the port on the case icon is
              -- the data source.
                (port, resPort)
              else (resPort, port)

combineExpressions :: Bool -> [(GraphAndRef, NameAndPort)] -> SyntaxGraph
combineExpressions inPattern portExpPairs =
  mconcat $ fmap makeGraph portExpPairs
  where
    edgeOpts = if inPattern then [EdgeInPattern] else []
    makeGraph (GraphAndRef graph ref, port) =
      graph <> case ref of
        Left str ->
          if inPattern
            then bindsToSyntaxGraph [SgBind str (Right port)]
            else sinksToSyntaxGraph [SgSink str port]
        Right resPort -> edgesToSyntaxGraph [Edge edgeOpts (resPort, port)]

makeApplyGraph ::
  Int ->
  LikeApplyFlavor ->
  Bool ->
  NodeName ->
  GraphAndRef ->
  [GraphAndRef] ->
  (SyntaxGraph, NameAndPort)
makeApplyGraph numArgs applyFlavor inPattern applyIconName funVal argVals =
  ( newGraph <> combinedGraph,
    nameAndPort applyIconName (resultPort applyNode)
  )
  where
    applyNode = ApplyNode applyFlavor numArgs
    argumentNamePorts =
      map (nameAndPort applyIconName) (argumentPorts applyNode)
    functionPort = nameAndPort applyIconName (inputPort applyNode)
    combinedGraph =
      combineExpressions inPattern $
        zip (funVal : argVals) (functionPort : argumentNamePorts)
    icons = [Named applyIconName (mkEmbedder applyNode)]
    newGraph = syntaxGraphFromNodes icons

makeMultiIfGraph ::
  Int ->
  NodeName ->
  [GraphAndRef] ->
  [GraphAndRef] ->
  (SyntaxGraph, NameAndPort)
makeMultiIfGraph numPairs multiIfName bools exps =
  (newGraph, nameAndPort multiIfName (resultPort multiIfNode))
  where
    multiIfNode = CaseOrMultiIfNode MultiIfTag numPairs
    expsWithPorts = zip exps $ map (nameAndPort multiIfName) multiIfRhsPorts
    boolsWithPorts = zip bools $ map (nameAndPort multiIfName) multiIfBoolPorts
    combindedGraph = combineExpressions False $ expsWithPorts <> boolsWithPorts
    icons = [Named multiIfName (mkEmbedder multiIfNode)]
    newGraph = syntaxGraphFromNodes icons <> combindedGraph

namesInPatternHelper :: GraphAndRef -> [String]
namesInPatternHelper (GraphAndRef graph ref) = case ref of
  Left str -> [str]
  Right _ -> sgBindToString <$> sgBinds graph

namesInPattern :: (GraphAndRef, Maybe String) -> [String]
namesInPattern (graphAndRef, mName) = case mName of
  Nothing -> otherNames
  Just n -> n : otherNames
  where
    otherNames = namesInPatternHelper graphAndRef

-- | Recursivly find the matching reference in a list of bindings.
-- TODO: Might want to present some indication if there is a reference cycle.
lookupReference :: [SgBind] -> Reference -> Reference
lookupReference _ ref@(Right _) = ref
lookupReference bindings ref@(Left originalS) = lookupHelper ref
  where
    lookupHelper newRef@(Right _) = newRef
    lookupHelper newRef@(Left s) = case lookup s (fmap sgBindToTuple bindings) of
      Just r -> failIfCycle r $ lookupHelper r
      Nothing -> newRef
      where
        failIfCycle r@(Left newStr) res = if newStr == originalS then r else res
        failIfCycle _ res = res

deleteBindings :: SyntaxGraph -> SyntaxGraph
deleteBindings (SyntaxGraph a b c _ e) = SyntaxGraph a b c mempty e

makeEdgesCore :: [SgSink] -> [SgBind] -> ([SgSink], [Edge])
makeEdgesCore sinks bindings = partitionEithers $ fmap renameOrMakeEdge sinks
  where
    renameOrMakeEdge :: SgSink -> Either SgSink Edge
    renameOrMakeEdge orig@(SgSink s destPort) =
      case lookup s (fmap sgBindToTuple bindings) of
        Just ref -> case lookupReference bindings ref of
          Right sourcePort -> Right $ makeSimpleEdge (sourcePort, destPort)
          Left newStr -> Left $ SgSink newStr destPort
        Nothing -> Left orig

makeEdges :: SyntaxGraph -> SyntaxGraph
makeEdges (SyntaxGraph icons edges sinks bindings eMap) = newGraph
  where
    (newSinks, newEdges) = makeEdgesCore sinks bindings
    newGraph = SyntaxGraph icons (newEdges <> edges) newSinks bindings eMap

makeBox :: String -> State IDState (SyntaxGraph, NameAndPort)
makeBox str = do
  name <- getUniqueName
  let graph =
        syntaxGraphFromNodes [Named name (mkEmbedder (LiteralNode str))]
  pure (graph, justName name)

nTupleString :: Int -> String
nTupleString n = '(' : replicate (n -1) ',' ++ ")"

-- TODO Unit tests for this
nTupleSectionString :: [Bool] -> String
nTupleSectionString bools = '(' : (commas ++ ")")
  where
    commas = case concatMap trueToUnderscore bools of
      [] -> []
      (_ : xs) -> xs

    trueToUnderscore x =
      if x
        then ",_"
        else ","

nListString :: Int -> String
-- TODO: Use something better than [_]
nListString 1 = "[_]"
nListString n = '[' : replicate (n -1) ',' ++ "]"

nodeToIcon :: EmbedderSyntaxNode -> Icon
nodeToIcon (Embedder embeddedNodes node) = case node of
  (ApplyNode flavor x) ->
    nestedApplySyntaxNodeToIcon flavor x embeddedNodes
  (PatternApplyNode s children) ->
    nestedPatternNodeToIcon s children
  (NameNode s) -> TextBoxIcon s
  (BindNameNode s) -> BindTextBoxIcon s
  (LiteralNode s) -> TextBoxIcon s
  (FunctionDefNode labels bodyNodes) ->
    nestedLambdaToIcon labels embeddedNodes bodyNodes
  CaseResultNode -> CaseResultIcon
  (CaseOrMultiIfNode tag x) ->
    nestedCaseOrMultiIfNodeToIcon tag x embeddedNodes

-- | Helper for makeArg
findArg :: Port -> (NodeName, Edge) -> Bool
findArg
  currentPort
  ( argName,
    Edge _ (NameAndPort fromName fromPort, NameAndPort toName toPort)
    )
    | argName == fromName = maybeBoolToBool $ fmap (== currentPort) toPort
    | argName == toName = maybeBoolToBool $ fmap (== currentPort) fromPort
    | otherwise = False -- This case should never happen

makeArg :: Set.Set (NodeName, Edge) -> Port -> Maybe NodeName
makeArg args port = fst <$> find (findArg port) args

nestedApplySyntaxNodeToIcon ::
  LikeApplyFlavor ->
  Int ->
  Set.Set (NodeName, Edge) ->
  Icon
nestedApplySyntaxNodeToIcon flavor numArgs args =
  NestedApply flavor headIcon argList
  where
    dummyNode = ApplyNode flavor numArgs
    argPorts = take numArgs (argumentPorts dummyNode)
    headIcon = makeArg args (inputPort dummyNode)
    argList = fmap (makeArg args) argPorts

nestedLambdaToIcon ::
  [String] -> -- labels
  Set.Set (NodeName, Edge) -> -- embedded icons
  Set.Set NodeName -> -- body nodes
  Icon
nestedLambdaToIcon labels embeddedNodes =
  LambdaIcon labels embeddedBodyNode
  where
    dummyNode = FunctionDefNode [] Set.empty
    embeddedBodyNode = makeArg embeddedNodes (inputPort dummyNode)

nestedCaseOrMultiIfNodeToIcon ::
  CaseOrMultiIfTag ->
  Int ->
  Set.Set (NodeName, Edge) ->
  Icon
nestedCaseOrMultiIfNodeToIcon tag numArgs args = case tag of
  CaseTag -> NestedCaseIcon argList
  MultiIfTag -> NestedMultiIfIcon argList
  where
    dummyNode = CaseOrMultiIfNode CaseTag numArgs
    argPorts = take (2 * numArgs) $ argumentPorts dummyNode
    argList = fmap (makeArg args) (inputPort dummyNode : argPorts)

nestedPatternNodeToIcon :: String -> [Labeled (Maybe SgNamedNode)] -> Icon
nestedPatternNodeToIcon str children =
  NestedPApp
    (pure (Just (Named (NodeName (-1)) (TextBoxIcon str))))
    -- Why so many fmaps?
    ((fmap . fmap . fmap . fmap) nodeToIcon children)

makeLNode :: SgNamedNode -> ING.LNode SgNamedNode
makeLNode namedNode@(Named (NodeName name) _) = (name, namedNode)

lookupInEmbeddingMap :: NodeName -> Map.Map NodeName NodeName -> NodeName
lookupInEmbeddingMap origName eMap = lookupHelper origName
  where
    lookupHelper name = case Map.lookup name eMap of
      Nothing -> name
      Just parent ->
        if parent == origName
          then
            error $
              "lookupInEmbeddingMap: Found cycle. Node = "
                ++ show origName
                ++ "\nEmbedding Map = "
                ++ show eMap
          else lookupHelper parent

syntaxGraphToFglGraph :: SyntaxGraph -> FGR.Gr SgNamedNode Edge
syntaxGraphToFglGraph (SyntaxGraph nodes edges _ _ eMap) =
  ING.mkGraph (fmap makeLNode nodes) labeledEdges
  where
    labeledEdges = fmap makeLabeledEdge edges

    makeLabeledEdge e@(Edge _ (NameAndPort name1 _, NameAndPort name2 _)) =
      ( nodeNameToInt $ lookupInEmbeddingMap name1 eMap,
        nodeNameToInt $ lookupInEmbeddingMap name2 eMap,
        e
      )
