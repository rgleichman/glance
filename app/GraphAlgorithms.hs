module GraphAlgorithms(
  collapseNodes
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive.Graph as ING
import Types(SgNamedNode, Edge, SyntaxNode(..), sgNamedNodeToSyntaxNode)
import Data.Maybe(listToMaybe)

import Util(printSelf)

-- See graph_algs.txt for pseudocode

type SyntaxGraph gr = gr SgNamedNode Edge

collapseNodes :: (ING.DynGraph gr) => SyntaxGraph gr -> gr SgNamedNode Edge
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
  parentsThatCannotEmbed = filter (graphNodeCanEmbed graph) (findParents graph node)

findParents :: ING.Graph gr => gr a b -> ING.Node -> [ING.Node]
-- TODO, may need to use ING.pre or ING.neighbors instead of ING.suc'
findParents = ING.suc

graphNodeCanEmbed :: SyntaxGraph gr -> ING.Node -> Bool
graphNodeCanEmbed graph node = syntaxNodeCanEmbed $ lookupSyntaxNode graph node

lookupSyntaxNode :: SyntaxGraph gr -> ING.Node -> SyntaxNode
lookupSyntaxNode = _

collapseRoots = _

-- TODO Remove unneeded code after here
collapseNodes' initialGraph = ING.ufold folder ING.empty initialGraph where
  folder context accumGraph = newGraph where
    newGraph
      -- Curnet node can not embed, and can not be embedded
      | not (nodeIsEmbeddable currentSyntaxNode) && not (syntaxNodeCanEmbed currentSyntaxNode) = context ING.& accumGraph
      | not (willBeEmbedded context initialGraph) = context ING.& accumGraph
      | otherwise = accumGraph
    currentSyntaxNode = extractSyntaxNode $ ING.labNode' context

-- | True if the node in the context will be embedded in another node
-- TODO: This case expression in willBeEmbedded is wrong and is a temporary shim for testing
willBeEmbedded :: (ING.Graph gr) => ING.Context SgNamedNode Edge -> SyntaxGraph gr-> Bool
willBeEmbedded context gr = nodeIsEmbeddable syntaxNode && parentCanEmbed
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

-- | A nodeIsEmbeddable if it can be collapsed into another node
nodeIsEmbeddable :: SyntaxNode -> Bool
nodeIsEmbeddable n = case n of
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
