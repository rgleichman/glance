module GraphAlgorithms(
  collapseNodes
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive.Graph as ING
import Types(SgNamedNode, Edge, SyntaxNode(..), sgNamedNodeToSyntaxNode)
import Data.Maybe(listToMaybe)

import Util(printSelf)

collapseNodes :: (ING.DynGraph gr) => gr SgNamedNode Edge -> gr SgNamedNode Edge
collapseNodes initialGraph = ING.ufold folder ING.empty initialGraph where
  folder context accumGraph = newGraph where
    newGraph
      -- Curnet node can not embed, and can not be embedded
      | not (nodeIsEmbeddable currentSyntaxNode) && not (nodeCanEmbed currentSyntaxNode) = context ING.& accumGraph
      | not (willBeEmbedded context initialGraph) = context ING.& accumGraph
      | otherwise = accumGraph
    currentSyntaxNode = extractSyntaxNode $ ING.labNode' context

-- | True if the node in the context will be embedded in another node
-- TODO: This case expression in willBeEmbedded is wrong and is a temporary shim for testing
willBeEmbedded :: (ING.Graph gr) => ING.Context SgNamedNode Edge -> gr SgNamedNode Edge-> Bool
willBeEmbedded context gr = nodeIsEmbeddable syntaxNode && parentCanEmbed
  where -- currentNodeEmbeddable && parentCanEmbed where
    currentNode = ING.labNode' context
    syntaxNode = printSelf $ extractSyntaxNode currentNode
    parentCanEmbed = maybeBoolToBool $ fmap (nodeCanEmbed . sgNamedNodeToSyntaxNode) (nodesParent gr context)

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

-- | A nodeCanEmbed if it can contain other nodes
nodeCanEmbed :: SyntaxNode -> Bool
nodeCanEmbed n = case n of
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
