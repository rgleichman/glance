module GraphAlgorithms(
  collapseNodes
  ) where

import qualified Data.Graph.Inductive.PatriciaTree as FGR
import qualified Data.Graph.Inductive.Graph as ING
import Types(SgNamedNode, Edge, SyntaxNode(..), sgNamedNodeToSyntaxNode)
import Data.Maybe(listToMaybe)

collapseNodes :: (ING.DynGraph gr) => gr SgNamedNode Edge -> gr SgNamedNode Edge
collapseNodes initialGraph = ING.ufold folder ING.empty initialGraph where
  folder context accumGraph = newGraph where
    newGraph
      | not (willBeEmbedded context initialGraph) = context ING.& accumGraph
      | otherwise = accumGraph

-- | True if the node in the context will be embedded in another node
willBeEmbedded :: (ING.Graph gr) => ING.Context SgNamedNode Edge -> gr SgNamedNode Edge-> Bool
willBeEmbedded context gr = currentNodeEmbeddable && parentCanEmbed where
  currentNode = ING.labNode' context
  currentNodeEmbeddable = nodeIsEmbeddable (extractSyntaxNode currentNode)
  parentCanEmbed = maybeBoolToBool $ fmap (nodeCanEmbed . sgNamedNodeToSyntaxNode) (nodesParent gr context)

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or

-- TODO make this work with pattern apply also
nodesParent :: (ING.Graph gr) => gr a b -> ING.Context a b -> Maybe a
nodesParent gr context =  listToMaybe (ING.pre' context) >>= ING.lab gr

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
