module GraphAlgorithms(
  collapseNodes,
  findTreeRoots,
  childCanBeEmbedded
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive.Graph as ING
import Types(SgNamedNode, Edge(..), SyntaxNode(..), sgNamedNodeToSyntaxNode, EdgeEnd(..), NameAndPort(..), IngSyntaxGraph)
import Data.Maybe(listToMaybe, catMaybes)
import Data.List(foldl', find)
import Diagrams.Prelude(toName)

import Util(printSelf)

-- See graph_algs.txt for pseudocode

type LabelledGraphEdge = ING.LEdge Edge

-- START collapseNodes helper functions --

findParents :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
-- TODO, may need to use ING.pre or ING.neighbors instead of ING.suc'
findParents = ING.suc

findChildren :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
findChildren = ING.pre

-- | graphNodeCanEmbed returns true if the label (SyntaxNode) associated with the
-- node can be embedded in other SyntaxNodes (i.e. nodeCanEmbed is True)
graphNodeCanEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
graphNodeCanEmbed graph node = maybeBoolToBool $ fmap syntaxNodeCanEmbed (lookupSyntaxNode graph node)

graphNodeIsEmbeddable :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
graphNodeIsEmbeddable graph node = maybeBoolToBool $ fmap syntaxNodeIsEmbeddable (lookupSyntaxNode graph node)

lookupSyntaxNode :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Maybe SyntaxNode
lookupSyntaxNode gr node = fmap sgNamedNodeToSyntaxNode $ ING.lab gr node

-- | filterNodes returns a list of the nodes in the graph
-- where the filter function is true.
filterNodes :: ING.DynGraph gr => (ING.Node -> Bool) -> gr a b -> [ING.Node]
filterNodes pred gr = ING.nodes $ ING.nfilter pred gr

-- | Replace the a node's label
changeNodeLabel :: ING.DynGraph gr => gr a b -> ING.Node -> a -> gr a b
changeNodeLabel graph node newLabel = case ING.match node graph of
  (Just (inEdges, _, _, outEdges), restOfTheGraph) -> (inEdges, node, newLabel, outEdges) ING.& restOfTheGraph
  (Nothing, _) -> graph

findEdgeLabel :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdgeLabel graph node1 node2 = fmap fst matchingEdges where
  labelledEdges = ING.lneighbors graph node1
  matchingEdges = find ((== node2) . snd) labelledEdges

-- END helper functions --

collapseNodes :: (ING.DynGraph gr) => IngSyntaxGraph gr -> IngSyntaxGraph gr
collapseNodes originalGraph = finalGraph where
  -- findTreeRoots returns a list of nodes that will embed other nodes, but are not embedded themselves.
  -- These nodes are thus each a root of a collapsed node tree.
  treeRoots = findTreeRoots originalGraph
  -- Now collapse each tree of nodes
  finalGraph = collapseRoots treeRoots originalGraph treeRoots

-- START findTreeRoots functions --

-- |findTreeRoots returns a list of nodes that might embed other nodes, but are not embedded themselves.
-- These nodes are thus each a root of a collapsed node tree.
-- A node is a treeRoot if all of these conditions are true:
-- 1. The SyntaxNode can embed other nodes (i.e. syntaxNodeCanEmbed is true)
-- 2. The node has no parents that can embed it
-- TODO These rules should be revised to allow cycles to be embedded.
-- Condition 2. should be revised such that if there is a parent that is a bind, it's a root even if other nodes can embed it.
-- Note: A treeRoot may not actually have any embeddable children, since collapseTree will do nothing in that case.
findTreeRoots :: ING.DynGraph gr => IngSyntaxGraph gr -> [ING.Node]
findTreeRoots graph = filterNodes (isTreeRoot graph) graph

isTreeRoot :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
isTreeRoot graph node = graphNodeCanEmbed graph node && noParentsCanEmbed where
  noParentsCanEmbed = null parentsThatCanEmbed
  parentsThatCanEmbed = filter (graphNodeCanEmbed graph) parents
  parents = findParents graph node

-- END findTreeRoots functions
-- START collapseRoots functions

collapseRoots :: ING.DynGraph gr => [ING.Node] -> IngSyntaxGraph gr -> [ING.Node] -> IngSyntaxGraph gr
collapseRoots treeRoots = foldl' (collapseTree treeRoots)

collapseTree :: ING.DynGraph gr => [ING.Node] -> IngSyntaxGraph gr -> ING.Node -> IngSyntaxGraph gr
collapseTree treeRoots oldGraph rootNode = case childrenToEmbed of
  [] -> oldGraph
  _ -> finalGraph
  where
    -- TODO Write code for subfunctions
    childrenToEmbed = findChildrenToEmbed treeRoots rootNode oldGraph
    -- Recursively collapse the children nodes
    graphWithCollapsedChildren = collapseRoots treeRoots oldGraph childrenToEmbed
      -- Modify the rootNode label (i.e. SyntaxNode) to incorporate the children it is embedding
    graphWithEmbeddedChildren = embedChildSyntaxNodes rootNode childrenToEmbed graphWithCollapsedChildren
    -- Transfer the edges of the children to rootNode
    childEdgesToTransfer = findChildEdgesToTransfer rootNode childrenToEmbed graphWithEmbeddedChildren
    graphWithEdgesTransferred = ING.insEdges childEdgesToTransfer graphWithEmbeddedChildren
    -- Delete the children that have been embedded (and any or their remaining edges)
    finalGraph = deleteChildren childrenToEmbed graphWithEdgesTransferred

-- | findChildrenToEmbed returns a list of the node's children that can be embedded
-- A child can be embedded iff all of these conditions are true:
-- 1. The node is not a treeRoot (otherwise a cycle of embedding could occur)
-- 2. The SyntaxNode is embeddable (i.e. syntaxNodeIsEmbeddable is True)
-- 3. The node has exactly one parent that can embed (i.e. nodeCanEmbed is True for one parent)
findChildrenToEmbed :: ING.Graph gr => [ING.Node] -> ING.Node -> IngSyntaxGraph gr -> [ING.Node]
findChildrenToEmbed treeRoots node graph = if graphNodeCanEmbed graph node
  then childrenToEmbed
  else []
  where
    childrenToEmbed = filter (childCanBeEmbedded treeRoots graph) (findChildren graph node)

childCanBeEmbedded :: ING.Graph gr => [ING.Node] -> IngSyntaxGraph gr -> ING.Node -> Bool
childCanBeEmbedded treeRoots graph child = notTreeRoot && isEmbeddable && oneParentCanEmbed where
  notTreeRoot = notElem child treeRoots
  isEmbeddable = graphNodeIsEmbeddable graph child
  oneParentCanEmbed = case parentsThatCanEmbed of
    [_] -> True
    _ -> False
  parentsThatCanEmbed = filter (graphNodeCanEmbed graph) (findParents graph child)

-- TODO findChildEdgesToTransfer might add too many edges
findChildEdgesToTransfer :: ING.Graph gr => ING.Node -> [ING.Node] -> gr a b-> [ING.LEdge b]
findChildEdgesToTransfer parentNode nodes graph = concatMap makeLabelledGraphEdges nodes where
  makeLabelledGraphEdges childNode = fmap (changeEdgeToParent parentNode childNode) $ ING.inn graph childNode

changeEdgeToParent :: ING.Node -> ING.Node -> ING.LEdge b -> ING.LEdge b
changeEdgeToParent parentNode childNode (fromNode, toNode, edgeLabel)
  | childNode == fromNode = (parentNode, toNode, edgeLabel)
  | childNode == toNode = (fromNode, parentNode, edgeLabel)

-- | Change the node label of the parent to be nested.
embedChildSyntaxNodes :: ING.DynGraph gr => ING.Node -> [ING.Node] -> IngSyntaxGraph gr -> IngSyntaxGraph gr
embedChildSyntaxNodes parentNode childrenNodes oldGraph = case childrenNodes of
  [] -> oldGraph
  _ -> newGraph
  where
    maybeOldNodeLabel = ING.lab oldGraph parentNode
    newGraph = case maybeOldNodeLabel of
      Nothing -> oldGraph
      Just oldNodeLabel -> changeNodeLabel oldGraph parentNode newNodeLabel
        where
          (nodeName, oldSyntaxNode) = oldNodeLabel
          newNodeLabel = (nodeName, newSyntaxNode)
          newSyntaxNode = case oldSyntaxNode of
            -- TODO Add PatternApplyNode, and NestedApplyNode
            ApplyNode x -> NestedApplyNode x childrenAndEdgesToParent
            _ -> oldSyntaxNode
    childrenAndEdgesToParent = catMaybes $ fmap findChildAndEdge childrenNodes
    findChildAndEdge childNode =
      (,) <$> ING.lab oldGraph childNode <*> findEdgeLabel oldGraph parentNode childNode


deleteChildren :: ING.Graph gr => [ING.Node] -> IngSyntaxGraph gr -> IngSyntaxGraph gr
deleteChildren = ING.delNodes

-- END collapseRoots functions

-- TODO Remove unneeded code after here
collapseNodes' initialGraph = ING.ufold folder ING.empty initialGraph where
  folder context accumGraph = newGraph where
    newGraph
      -- Curnet node can not embed, and can not be embedded
      | not (syntaxNodeIsEmbeddable currentSyntaxNode) && not (syntaxNodeCanEmbed currentSyntaxNode) = context ING.& accumGraph
      | not (willBeEmbedded context initialGraph) = context ING.& accumGraph
      | otherwise = accumGraph
    currentSyntaxNode = extractSyntaxNode $ ING.labNode' context

-- | True if the node in the context will be embedded in another node
-- TODO: This case expression in willBeEmbedded is wrong and is a temporary shim for testing
willBeEmbedded :: (ING.Graph gr) => ING.Context SgNamedNode Edge -> IngSyntaxGraph gr-> Bool
willBeEmbedded context gr = syntaxNodeIsEmbeddable syntaxNode && parentCanEmbed
  where -- currentNodeEmbeddable && parentCanEmbed where
    currentNode = ING.labNode' context
    syntaxNode = printSelf $ extractSyntaxNode currentNode
    parentCanEmbed = maybeBoolToBool $ fmap (syntaxNodeCanEmbed . sgNamedNodeToSyntaxNode) (nodesParent gr context)

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or


-- TODO make this work with pattern apply also
nodesParent :: (ING.Graph gr) => gr a b -> ING.Context a b -> Maybe a
nodesParent gr context =  listToMaybe connectedNodes >>= ING.lab gr where
  -- The nodes parent may have already been removed from context, so need to look
  -- up neighbors in the original graph.
  originalContext = ING.context gr (ING.node' context)
  -- TODO, may need to use ING.pre' or ING.neighbors' instead of ING.suc'
  connectedNodes = printSelf (ING.suc' originalContext)

-- | A syntaxNodeIsEmbeddable if it can be collapsed into another node
syntaxNodeIsEmbeddable :: SyntaxNode -> Bool
syntaxNodeIsEmbeddable n = case n of
  ApplyNode _ -> True
  -- TODO make PatternApplyNode embeddable
  PatternApplyNode _ _ -> False
  NameNode _ -> False
  LiteralNode _ -> True
  FunctionDefNode _ -> False
  GuardNode _ -> False
  CaseNode _ -> False
  BranchNode -> False
  CaseResultNode -> False
  -- Don't use a catch all (i.e. irrefutable) pattern here so that if other
  -- SyntaxNodes are added we will get a warning here.

-- | A syntaxNodeCanEmbed if it can contain other nodes
syntaxNodeCanEmbed :: SyntaxNode -> Bool
syntaxNodeCanEmbed n = case n of
  ApplyNode _ -> True
  -- TODO make PatternApplyNode embed
  PatternApplyNode _ _ -> False
  NameNode _ -> False
  LiteralNode _ -> False
  FunctionDefNode _ -> False
  GuardNode _ -> False
  CaseNode _ -> False
  BranchNode -> False
  CaseResultNode -> False
  -- Don't use a catch all (i.e. irrefutable) pattern here so that if other
  -- SyntaxNodes are added we will get a warning here.

extractSyntaxNode :: ING.LNode SgNamedNode -> SyntaxNode
extractSyntaxNode = snd . snd
