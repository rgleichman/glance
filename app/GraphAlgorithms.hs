module GraphAlgorithms(
  ParentType(..),
  collapseNodes,
  findTreeRoots,
  nodeWillBeEmbedded
  ) where

import qualified Data.Graph.Inductive as ING

import Data.List(foldl', find)
import Data.Maybe(catMaybes, isJust, fromMaybe)
--import qualified Debug.Trace

import Types(SyntaxNode(..), sgNamedNodeToSyntaxNode, IngSyntaxGraph, Edge(..))
import Util(maybeBoolToBool)
--import Util(printSelf)

-- See graph_algs.txt for pseudocode

data ParentType = ApplyParent | NotAParent

-- START HELPER functions --

-- | A syntaxNodeIsEmbeddable if it can be collapsed into another node
syntaxNodeIsEmbeddable :: ParentType -> SyntaxNode -> Bool
syntaxNodeIsEmbeddable parentType n = case (parentType, n) of
  (ApplyParent, LikeApplyNode _ _) -> True 
  (ApplyParent, LiteralNode _) -> True
  _ -> False

-- | A syntaxNodeCanEmbed if it can contain other nodes
syntaxNodeCanEmbed :: SyntaxNode -> Bool
syntaxNodeCanEmbed n = case n of
  LikeApplyNode _ _ -> True
  NestedApplyNode _ _ _ -> True -- This case should not happen
  _ -> False

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode n = case n of
  LikeApplyNode _ _ -> ApplyParent
  NestedApplyNode _ _ _ -> ApplyParent
  -- The NotAParent case should never occur.
  _ -> NotAParent

findParents :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> [ING.Node]
findParents graph node = filter parentFilter $  ING.suc graph node where
  parentFilter parentNode = parentNode /= node

findChildren :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
findChildren = ING.pre

-- | graphNodeCanEmbed returns true if the label (SyntaxNode) associated with the
-- node can be embedded in other SyntaxNodes (i.e. nodeCanEmbed is True)
graphNodeCanEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
graphNodeCanEmbed graph node = maybeBoolToBool $ fmap syntaxNodeCanEmbed (lookupSyntaxNode graph node)

graphNodeIsEmbeddable :: ING.Graph gr => ParentType -> IngSyntaxGraph gr -> ING.Node -> Bool
graphNodeIsEmbeddable parentType graph node = maybeBoolToBool $ fmap (syntaxNodeIsEmbeddable parentType) (lookupSyntaxNode graph node)

lookupSyntaxNode :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Maybe SyntaxNode
lookupSyntaxNode gr node = sgNamedNodeToSyntaxNode <$> ING.lab gr node

lookupParentType :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ParentType
lookupParentType graph node = fromMaybe NotAParent $ parentTypeForNode <$> lookupSyntaxNode graph node

-- | filterNodes returns a list of the nodes in the graph
-- where the filter function is true.
filterNodes :: ING.DynGraph gr => (ING.Node -> Bool) -> gr a b -> [ING.Node]
filterNodes condition gr = ING.nodes $ ING.nfilter condition gr

-- | Replace the a node's label
changeNodeLabel :: ING.DynGraph gr => gr a b -> ING.Node -> a -> gr a b
changeNodeLabel graph node newLabel = case ING.match node graph of
  (Just (inEdges, _, _, outEdges), restOfTheGraph) -> (inEdges, node, newLabel, outEdges) ING.& restOfTheGraph
  (Nothing, _) -> graph

findEdgeLabel :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdgeLabel graph node1 node2 = fmap fst matchingEdges where
  labelledEdges = ING.lneighbors graph node1
  matchingEdges = find ((== node2) . snd) labelledEdges

findParentsThatCanEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> [ING.Node]
findParentsThatCanEmbed graph child = filter parentFilter (findParents graph child) where
  parentFilter parentNode = graphNodeCanEmbed graph parentNode && graphNodeIsEmbeddable parentType graph child where
    parentType = lookupParentType graph parentNode

-- | Finds the first edge from the first node to the second node
findEdge :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdge graph fromNode toNode = lookup toNode $ ING.lsuc graph fromNode

parentIsOnlyEdge :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ING.Node -> Bool
parentIsOnlyEdge graph parent child = case findEdge graph child parent of
  Nothing -> error "parentIsOnlyEdge: There is no edge from the child to the parent."
  Just edge -> numEdges == 1 where
    (childNamePort, _) = edgeConnection edge
    edgeLabels = filter (childNamePort ==) $ (fst . edgeConnection . snd) <$> ING.lsuc graph child
    numEdges = length edgeLabels
    
findParentThatWillEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Maybe ING.Node
findParentThatWillEmbed graph child =
  case findParentsThatCanEmbed graph child of
    [parent] -> if parentIsOnlyEdge graph parent child
      then Just parent
      else Nothing
    _ -> Nothing

-- END helper functions --

collapseNodes :: (ING.DynGraph gr) => IngSyntaxGraph gr -> IngSyntaxGraph gr
collapseNodes originalGraph = finalGraph where
  -- findTreeRoots returns a list of nodes that will embed other nodes, but are not embedded themselves.
  -- These nodes are thus each a root of a collapsed node tree.
  treeRoots = findTreeRoots originalGraph
  -- Now collapse each tree of nodes
  finalGraph = collapseRoots originalGraph originalGraph treeRoots

-- START findTreeRoots functions --

-- |findTreeRoots returns a list of nodes that might embed other nodes, but are not embedded themselves.
-- These nodes are thus each a root of a collapsed node tree.
-- A node is a treeRoot if all of these conditions are true:
-- 1. The SyntaxNode can embed other nodes (i.e. syntaxNodeCanEmbed is true)
-- 2. The node will not be embedded.
-- Note: A treeRoot may not actually have any embeddable children, since collapseTree will do nothing in that case.
findTreeRoots :: ING.DynGraph gr => IngSyntaxGraph gr -> [ING.Node]
findTreeRoots graph = filterNodes isTreeRoot graph where
  isTreeRoot node = graphNodeCanEmbed graph node && not (nodeWillBeEmbedded graph node)

nodeWillBeEmbedded :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
nodeWillBeEmbedded graph node = isJust $ findParentThatWillEmbed graph node

-- END findTreeRoots functions
-- START collapseRoots functions

collapseRoots :: ING.DynGraph gr => IngSyntaxGraph gr -> IngSyntaxGraph gr -> [ING.Node] -> IngSyntaxGraph gr
collapseRoots originalGraph = foldl' (collapseTree originalGraph)

collapseTree :: ING.DynGraph gr => IngSyntaxGraph gr -> IngSyntaxGraph gr -> ING.Node -> IngSyntaxGraph gr
collapseTree originalGraph oldGraph rootNode = case childrenToEmbed of
  [] -> oldGraph
  _ -> finalGraph
  where
    -- Need to use the original graph for finding children, otherwise a node can be embedded when it is used twice in
    -- what will be a single embedding node. Examples:
    --  "y = foo (3 + bazOf2) bazOf2 where bazOf2 = baz 2",
    --  "y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2"
    childrenToEmbed = findChildrenToEmbed rootNode originalGraph
    -- Recursively collapse the children nodes
    graphWithCollapsedChildren = collapseRoots originalGraph oldGraph childrenToEmbed
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
findChildrenToEmbed :: ING.Graph gr => ING.Node -> IngSyntaxGraph gr -> [ING.Node]
findChildrenToEmbed node graph = if graphNodeCanEmbed graph node
  then childrenToEmbed
  else []
  where
    childrenToEmbed = filter (childCanBeEmbedded node graph) (findChildren graph node)

childCanBeEmbedded :: ING.Graph gr => ING.Node -> IngSyntaxGraph gr -> ING.Node -> Bool
childCanBeEmbedded parentNode graph child =
  maybeBoolToBool $ (== parentNode) <$> findParentThatWillEmbed graph child

findChildEdgesToTransfer :: ING.Graph gr => ING.Node -> [ING.Node] -> gr a b-> [ING.LEdge b]
findChildEdgesToTransfer parentNode nodes graph = concatMap makeLabelledGraphEdges nodes where
  makeLabelledGraphEdges childNode = changeEdgeToParent parentNode childNode <$>
    ING.inn graph childNode
    ++
    filter (not. edgeGoesToParent parentNode) (ING.out graph childNode)

edgeGoesToParent :: ING.Node -> ING.LEdge b -> Bool
edgeGoesToParent parentNode (fromNode, toNode, _)
  | parentNode == fromNode = True
  | parentNode == toNode = True
  | otherwise = False

changeEdgeToParent :: ING.Node -> ING.Node -> ING.LEdge b -> ING.LEdge b
changeEdgeToParent parentNode childNode lEdge@(fromNode, toNode, edgeLabel)
  | childNode == fromNode = (parentNode, toNode, edgeLabel)
  | childNode == toNode = (fromNode, parentNode, edgeLabel)
  | otherwise = lEdge

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
            LikeApplyNode flavor x -> NestedApplyNode flavor x childrenAndEdgesToParent
            _ -> oldSyntaxNode
    childrenAndEdgesToParent = catMaybes $ fmap findChildAndEdge childrenNodes
    findChildAndEdge childNode =
      (,) <$> ING.lab oldGraph childNode <*> findEdgeLabel oldGraph parentNode childNode


deleteChildren :: ING.Graph gr => [ING.Node] -> IngSyntaxGraph gr -> IngSyntaxGraph gr
deleteChildren = ING.delNodes

-- END collapseRoots functions
