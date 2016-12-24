module GraphAlgorithms(
  ParentType(..),
  collapseNodes,
  findTreeRoots,
  nodeWillBeEmbedded
  ) where

import qualified Data.Graph.Inductive as ING

import qualified Control.Arrow as Arrow
import Data.List(foldl', find)
import Data.Maybe(catMaybes, isJust, fromMaybe)
--import qualified Debug.Trace

import Types(SyntaxNode(..), sgNamedNodeToSyntaxNode, IngSyntaxGraph, Edge(..),
            CaseOrGuardTag(..), Port(..), NameAndPort(..))
import Util(maybeBoolToBool)
--import Util(printSelf)

-- See graph_algs.txt for pseudocode

data ParentType = ApplyParent | CaseOrGuardParent | NotAParent deriving (Eq, Show)

data DirectionalEdge a = ParentToChild a | ChildToParent a deriving (Eq, Show)

-- START HELPER functions --

unwrapDirectionalEdge :: DirectionalEdge a -> a
unwrapDirectionalEdge d = case d of
  ParentToChild e -> e
  ChildToParent e -> e

-- | A syntaxNodeIsEmbeddable if it can be collapsed into another node
syntaxNodeIsEmbeddable :: ParentType -> SyntaxNode -> Maybe Port -> Bool
syntaxNodeIsEmbeddable parentType n mParentPort = case (parentType, n) of
  (ApplyParent, LikeApplyNode _ _) -> notResultPort
  (ApplyParent, LiteralNode _) -> notResultPort
  (CaseOrGuardParent, LiteralNode _) -> notResultPort
  _ -> False
  where
    notResultPort = case mParentPort of
      -- TODO Don't use hardcoded port number
      Just (Port 1) -> False
      _ -> True

-- | A syntaxNodeCanEmbed if it can contain other nodes
syntaxNodeCanEmbed :: SyntaxNode -> Bool
syntaxNodeCanEmbed = (NotAParent /=) . parentTypeForNode

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode n = case n of
  LikeApplyNode _ _ -> ApplyParent
  NestedApplyNode _ _ _ -> ApplyParent
  CaseNode _ -> CaseOrGuardParent
  GuardNode _ -> CaseOrGuardParent
  NestedCaseOrGuardNode _ _ _ -> CaseOrGuardParent
  -- The NotAParent case should never occur.
  _ -> NotAParent

findNeighbors :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> [ING.Node]
findNeighbors graph node = filter parentFilter $  ING.neighbors graph node where
  parentFilter parentNode = parentNode /= node

findParentsWithEdges :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ING.Adj (DirectionalEdge Edge)
findParentsWithEdges graph node = filter parentFilter adjacencies where
  parentFilter (_, parentNode) = parentNode /= node
  (incomingEdges, _, _, outgoingEdges) = ING.context graph node
  mappedIncomingEdges = fmap (Arrow.first ParentToChild) incomingEdges
  mappedOutgoingEdges = fmap (Arrow.first ChildToParent) outgoingEdges
  adjacencies = mappedIncomingEdges ++ mappedOutgoingEdges

-- | graphNodeCanEmbed returns true if the label (SyntaxNode) associated with the
-- node can be embedded in other SyntaxNodes (i.e. nodeCanEmbed is True)
graphNodeCanEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Bool
graphNodeCanEmbed graph node = maybeBoolToBool $ fmap syntaxNodeCanEmbed (lookupSyntaxNode graph node)

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

parentCanEmbedChild :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ING.Node -> DirectionalEdge Edge -> Bool
parentCanEmbedChild graph parent child directionalEdge = case lookupSyntaxNode graph child of
  Nothing -> False
  Just childSyntaxNode -> syntaxNodeIsEmbeddable parentType childSyntaxNode parentPort where
    parentType = lookupParentType graph parent
    parentPort = case directionalEdge of
      ParentToChild edge -> port where
        (NameAndPort _ port, _) = edgeConnection edge
      ChildToParent edge -> port where
        (_, NameAndPort _ port) = edgeConnection edge

findParentsThatCanEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ING.Adj (DirectionalEdge Edge)
findParentsThatCanEmbed graph child = filter parentFilter (findParentsWithEdges graph child) where
  parentFilter (directionalEdge, parentNode) = graphNodeCanEmbed graph parentNode && parentCanEmbedChild graph parentNode child directionalEdge

-- | Finds the first edge from the first node to the second node
findEdge :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdge graph fromNode toNode = lookup toNode $ ING.lsuc graph fromNode

parentIsOnlyEdge :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ING.Node -> Bool
parentIsOnlyEdge graph parent child = case findEdge graph child parent of
  Nothing -> case findEdge graph parent child of
    Nothing -> error "parentIsOnlyEdge: There is no edge from the child to the parent."
    -- TODO Finish this case
    Just edge -> numEdges == 1 where
      (parentNamePort, _) = edgeConnection edge
      edgeLabels = filter (parentNamePort ==) $ (fst . edgeConnection . snd) <$> ING.lsuc graph parent
      numEdges = length edgeLabels
  Just edge -> numEdges == 1 where
    (childNamePort, _) = edgeConnection edge
    edgeLabels = filter (childNamePort ==) $ (fst . edgeConnection . snd) <$> ING.lsuc graph child
    numEdges = length edgeLabels
    
findParentThatWillEmbed :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> Maybe (DirectionalEdge Edge, ING.Node)
findParentThatWillEmbed graph child =
  case findParentsThatCanEmbed graph child of
    [parent] -> if parentIsOnlyEdge graph (snd parent) child
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
    childrenToEmbedWithEdges = findChildrenToEmbed rootNode originalGraph
    childrenToEmbed = fmap snd childrenToEmbedWithEdges
    -- Recursively collapse the children nodes
    graphWithCollapsedChildren = collapseRoots originalGraph oldGraph childrenToEmbed
      -- Modify the rootNode label (i.e. SyntaxNode) to incorporate the children it is embedding
    graphWithEmbeddedChildren = embedChildSyntaxNodes rootNode childrenToEmbed graphWithCollapsedChildren
    -- Transfer the edges of the children to rootNode
    childEdgesToTransfer = findChildEdgesToTransfer rootNode childrenToEmbedWithEdges graphWithEmbeddedChildren
    graphWithEdgesTransferred = ING.insEdges childEdgesToTransfer graphWithEmbeddedChildren
    -- Delete the children that have been embedded (and any or their remaining edges)
    finalGraph = deleteChildren childrenToEmbed graphWithEdgesTransferred

-- | findChildrenToEmbed returns a list of the node's children that can be embedded
-- A child can be embedded iff all of these conditions are true:
-- 1. The node is not a treeRoot (otherwise a cycle of embedding could occur)
-- 2. The SyntaxNode is embeddable (i.e. syntaxNodeIsEmbeddable is True)
-- 3. The node has exactly one parent that can embed (i.e. nodeCanEmbed is True for one parent)
findChildrenToEmbed :: ING.Graph gr => ING.Node -> IngSyntaxGraph gr -> ING.Adj (DirectionalEdge Edge)-- [ING.Node]
findChildrenToEmbed node graph = if graphNodeCanEmbed graph node
  then childrenToEmbed
  else []
  where
    childrenToEmbed = catMaybes $  fmap (childCanBeEmbedded node graph) (findNeighbors graph node)

childCanBeEmbedded :: ING.Graph gr => ING.Node -> IngSyntaxGraph gr -> ING.Node -> Maybe (DirectionalEdge Edge, ING.Node)
childCanBeEmbedded parentNode graph child = case
  findParentThatWillEmbed graph child of
    Nothing -> Nothing
    Just (edge, childsParent) -> if childsParent == parentNode
      then Just (edge, child)
      else Nothing

edgesNotEqual :: Eq b => DirectionalEdge b -> ING.LEdge b -> Bool
edgesNotEqual dirEdge (_, _, e) = e /= unwrapDirectionalEdge dirEdge

findChildEdgesToTransfer :: (Eq b, ING.Graph gr) =>
  ING.Node -> ING.Adj (DirectionalEdge b) -> gr a b-> [ING.LEdge b]
findChildEdgesToTransfer parentNode childrenToEmbed graph = concatMap makeLabelledGraphEdges childrenToEmbed where
  makeLabelledGraphEdges (directionalParentToChildEdge, childNode) = changeEdgeToParent parentNode childNode <$>
    filter (edgesNotEqual directionalParentToChildEdge) (ING.inn graph childNode ++ ING.out graph childNode)

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
            CaseNode x -> NestedCaseOrGuardNode CaseTag x childrenAndEdgesToParent
            GuardNode x -> NestedCaseOrGuardNode GuardTag x childrenAndEdgesToParent
            _ -> oldSyntaxNode
    childrenAndEdgesToParent = catMaybes $ fmap findChildAndEdge childrenNodes
    findChildAndEdge childNode =
      (,) <$> ING.lab oldGraph childNode <*> findEdgeLabel oldGraph parentNode childNode


deleteChildren :: ING.Graph gr => [ING.Node] -> IngSyntaxGraph gr -> IngSyntaxGraph gr
deleteChildren = ING.delNodes

-- END collapseRoots functions
