module GraphAlgorithms(
  collapseNodes
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive.Graph as ING
import Types(SgNamedNode, Edge, SyntaxNode(..), sgNamedNodeToSyntaxNode)
import Data.Maybe(listToMaybe)
import Data.List(foldl')

import Util(printSelf)

-- See graph_algs.txt for pseudocode

type SyntaxGraph gr = gr SgNamedNode Edge

collapseNodes :: (ING.DynGraph gr) => SyntaxGraph gr -> SyntaxGraph gr
collapseNodes originalGraph = originalGraph where
  -- findTreeRoots returns a list of nodes that will embed other nodes, but are not embedded themselves.
  -- These nodes are thus each a root of a collapsed node tree.
  treeRoots = findTreeRoots originalGraph
  -- Now collapse each tree of nodes
  finalGraph = collapseRoots treeRoots originalGraph treeRoots

-- |findTreeRoots returns a list of nodes that might embed other nodes, but are not embedded themselves.
-- These nodes are thus each a root of a collapsed node tree.
-- A node is a treeRoot if all of these conditions are true:
-- 1. The SyntaxNode can embed other nodes (i.e. syntaxNodeCanEmbed is true)
-- 2. The node has at least one parent that can not embed (i.e. the node has a parent where syntaxNodeCanEmbed is false.)
-- Note: A treeRoot may not actually have any embeddable children, since collapseTree will do nothing in that case.
findTreeRoots :: ING.DynGraph gr => SyntaxGraph gr -> [ING.Node]
findTreeRoots graph = filterNodes (isTreeRoot graph) graph

-- filterNodes returns a list of the nodes in the graph
-- where the filter function is true.
filterNodes :: ING.DynGraph gr => (ING.Node -> Bool) -> gr a b -> [ING.Node]
filterNodes pred gr = ING.nodes $ ING.nfilter pred gr

isTreeRoot :: ING.Graph gr => SyntaxGraph gr -> ING.Node -> Bool
isTreeRoot graph node = graphNodeCanEmbed graph node && hasAParentThatCannotEmbed where
  hasAParentThatCannotEmbed = not $ null parentsThatCannotEmbed
  parentsThatCannotEmbed = filter (not . graphNodeCanEmbed graph) (findParents graph node)

findParents :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
-- TODO, may need to use ING.pre or ING.neighbors instead of ING.suc'
findParents = ING.suc

findChildren :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
findChildren = ING.pre

-- | graphNodeCanEmbed returns true if the label (SyntaxNode) associated with the
-- node can be embedded in other SyntaxNodes (i.e. nodeCanEmbed is True)
graphNodeCanEmbed :: ING.Graph gr => SyntaxGraph gr -> ING.Node -> Bool
graphNodeCanEmbed graph node = maybeBoolToBool $ fmap syntaxNodeCanEmbed (lookupSyntaxNode graph node)

graphNodeIsEmbeddable :: ING.Graph gr => SyntaxGraph gr -> ING.Node -> Bool
graphNodeIsEmbeddable graph node = maybeBoolToBool $ fmap syntaxNodeIsEmbeddable (lookupSyntaxNode graph node)


lookupSyntaxNode :: ING.Graph gr => SyntaxGraph gr -> ING.Node -> Maybe SyntaxNode
lookupSyntaxNode gr node = fmap sgNamedNodeToSyntaxNode $ ING.lab gr node

collapseRoots :: ING.Graph gr => [ING.Node] -> SyntaxGraph gr -> [ING.Node] -> SyntaxGraph gr
collapseRoots treeRoots = foldl' (collapseTree treeRoots)

collapseTree :: ING.Graph gr => [ING.Node] -> SyntaxGraph gr -> ING.Node -> SyntaxGraph gr
collapseTree treeRoots oldGraph rootNode = case childrenToEmbed of
  [] -> oldGraph
  _ -> finalGraph
  where
    -- TODO Write pseudocode for subfunctions
    childrenToEmbed = findChildrenToEmbed treeRoots rootNode oldGraph
    -- Recursively collapse the children nodes
    graphWithCollapsedChildren = collapseRoots treeRoots oldGraph childrenToEmbed
    -- Transfer the edges of the children to rootNode
    childEdgesToTransfer = findChildEdgesToTransfer childrenToEmbed graphWithCollapsedChildren
    graphWithChildEdgesDeleted = deleteChildEdges childEdgesToTransfer graphWithCollapsedChildren
    graphWithEdgesTransferred = addChildEdges rootNode childEdgesToTransfer graphWithChildEdgesDeleted
    -- Modify the rootNode label (i.e. SyntaxNode) to incorporate the children it is embedding
    graphWithChildrenCollapsed = embedChildSyntaxNodes rootNode childrenToEmbed graphWithEdgesTransferred
    -- Delete the children that have been embedded
    finalGraph = deleteChildren childrenToEmbed graphWithChildrenCollapsed

-- | findChildrenToEmbed returns a list of the node's children that can be embedded
-- A child can be embedded iff all of these conditions are true:
-- 1. The node is not a treeRoot (otherwise a cycle of embedding could occur)
-- 2. The SyntaxNode is embeddable (i.e. syntaxNodeIsEmbeddable is True)
-- 3. The node has exactly one parent that can embed (i.e. nodeCanEmbed is True for one parent)
findChildrenToEmbed :: ING.Graph gr => [ING.Node] -> ING.Node -> SyntaxGraph gr -> [ING.Node]
findChildrenToEmbed treeRoots node graph = if graphNodeCanEmbed graph node
  then childrenToEmbed
  else []
  where
    childrenToEmbed = filter (childCanBeEmbedded treeRoots graph) (findChildren graph node)

-- TODO Add type
childCanBeEmbedded treeRoots graph child = notTreeRoot && isEmbeddable && oneParentCanEmbed where
  notTreeRoot = notElem child treeRoots
  isEmbeddable = graphNodeIsEmbeddable graph child
  oneParentCanEmbed = case parentsThatCanEmbed of
    [_] -> True
    _ -> False
  parentsThatCanEmbed = filter (graphNodeCanEmbed graph) (findParents graph child)


type LabelledGraphEdge = ING.LEdge Edge

findChildEdgesToTransfer :: [ING.Node] -> SyntaxGraph gr -> [LabelledGraphEdge]
findChildEdgesToTransfer _ _ = [] -- TODO

deleteChildEdges :: [LabelledGraphEdge] -> SyntaxGraph gr -> SyntaxGraph gr
deleteChildEdges _ = id -- TODO

addChildEdges :: ING.Node -> [LabelledGraphEdge] -> SyntaxGraph gr -> SyntaxGraph gr
addChildEdges _ _ = id -- TODO

embedChildSyntaxNodes :: ING.Node -> [ING.Node] -> SyntaxGraph gr -> SyntaxGraph gr
embedChildSyntaxNodes _ _ = id -- TODO

deleteChildren :: [ING.Node] -> SyntaxGraph gr -> SyntaxGraph gr
deleteChildren _ = id -- TODO

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
willBeEmbedded :: (ING.Graph gr) => ING.Context SgNamedNode Edge -> SyntaxGraph gr-> Bool
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
  PatternApplyNode _ _ -> True
  NameNode _ -> True
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
  PatternApplyNode _ _ -> True
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
