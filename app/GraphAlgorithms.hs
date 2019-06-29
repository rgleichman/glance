{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
module GraphAlgorithms(
  ParentType(..),
  annotateGraph,
  collapseAnnotatedGraph
  ) where

import qualified Data.Graph.Inductive as ING
import Data.List(foldl', find)
import Data.Tuple(swap)
import GHC.Stack(HasCallStack)

import Constants(pattern ResultPortConst, pattern InputPortConst)
import Types(SyntaxNode(..), IngSyntaxGraph, Edge(..),
             CaseOrMultiIfTag(..), Port(..), NameAndPort(..), SgNamedNode(..)
            , AnnotatedGraph, EmbedInfo(..), EmbedDirection(..), NodeInfo(..))
import Util(sgNamedNodeToSyntaxNode)

{-# ANN module "HLint: ignore Use record patterns" #-}

data ParentType = ApplyParent
                | CaseParent
                | MultiIfParent
                | LambdaParent
                | NotAParent
  deriving (Eq, Show)

-- Helper functions

parentAndChild :: EmbedDirection
               -> (a, a) -- ^ (from, to)
               -> (a, a) -- ^ (parent, child)
parentAndChild embedDirection
  = case embedDirection of
      EdEmbedTo -> id
      EdEmbedFrom -> swap

-- End helper functions
-- START annotateGraph --

-- | A syntaxNodeIsEmbeddable if it can be collapsed into another node
syntaxNodeIsEmbeddable :: ParentType
                       -> SyntaxNode
                       -> Maybe Port
                       -> Maybe Port
                       -> Bool
syntaxNodeIsEmbeddable parentType syntaxNode mParentPort mChildPort
  = case (parentType, syntaxNode) of
      (ApplyParent, ApplyNode _ _ _) -> parentPortNotResult
      (ApplyParent, LiteralNode _) -> parentPortNotResult
      (ApplyParent, FunctionDefNode _ _ _)
        -> isInput mParentPort && isResult mChildPort

      -- (LambdaParent, ApplyNode _ _ _) -> parentPortIsInput
      -- (LambdaParent, LiteralNode _) -> parentPortIsInput
      -- (LambdaParent, FunctionDefNode _ _ _)
      --   -> parentPortIsInput

      (CaseParent, LiteralNode _) -> parentPortNotResult
      (CaseParent, ApplyNode _ _ _)
        -> parentPortNotResult && parentPortNotInput
      (CaseParent, PatternApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput

      (MultiIfParent, LiteralNode _) -> parentPortNotResult
      (MultiIfParent, ApplyNode _ _ _)
        -> parentPortNotResult && parentPortNotInput

      _ -> False
  where
    isInput mPort = case mPort of
      Just InputPortConst -> True
      _ -> False

    isResult mPort = case mPort of
      Nothing -> True
      Just ResultPortConst -> True
      Just _ -> False

    -- parentPortIsInput = isInput mParentPort

    parentPortNotInput = not $ isInput mParentPort
    parentPortNotResult = not $ isResult mParentPort

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode n = case n of
  ApplyNode _ _ _ -> ApplyParent
  CaseOrMultiIfNode CaseTag _ _ -> CaseParent
  CaseOrMultiIfNode MultiIfTag _ _ -> MultiIfParent
  FunctionDefNode _ _ _ -> LambdaParent
  _ -> NotAParent

lookupSyntaxNode :: ING.Graph gr =>
  IngSyntaxGraph gr -> ING.Node -> Maybe SyntaxNode
lookupSyntaxNode gr node = sgNamedNodeToSyntaxNode <$> ING.lab gr node

lookupParentType :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ParentType
lookupParentType graph node
  = maybe NotAParent parentTypeForNode $ lookupSyntaxNode graph node

{-# ANN edgeIsSingular "HLint: ignore Redundant bracket" #-}
edgeIsSingular :: ING.Graph gr => gr a Edge -> ING.Node -> Edge -> Bool
edgeIsSingular graph node edge = numEdges <= 1 where
  (childNamePort, _) = edgeConnection edge
  edgeLabels = filter
               (childNamePort ==)
               ((fst . edgeConnection . snd) <$> ING.lsuc graph node)
  numEdges = length edgeLabels

parentCanEmbedChild :: ING.Graph gr =>
  IngSyntaxGraph gr -> ING.Node -> ING.Node -> Edge -> EmbedDirection -> Bool
parentCanEmbedChild graph parent child edge embedDirection
  = case lookupSyntaxNode graph child of
      Nothing -> False
      Just childSyntaxNode ->
        edgeIsSingular graph child edge
        && syntaxNodeIsEmbeddable
        parentType
        childSyntaxNode
        parentPort
        childPort
        where
          parentType = lookupParentType graph parent
          (NameAndPort _ fromPort, NameAndPort _ toPort) = edgeConnection edge
          (parentPort, childPort)
            = parentAndChild embedDirection (fromPort, toPort)

findEmbedDir :: ING.Graph gr
             => IngSyntaxGraph gr
             -> ING.Node
             -> ING.Node
             -> Edge
             -> Maybe EmbedDirection
findEmbedDir gr fromNode toNode e = if
  | parentCanEmbedChild gr fromNode toNode e EdEmbedTo
    -> Just EdEmbedTo
  | parentCanEmbedChild gr toNode fromNode e EdEmbedFrom
    -> Just EdEmbedFrom
  | otherwise -> Nothing


annotateGraph :: ING.DynGraph gr => IngSyntaxGraph gr -> gr SgNamedNode (EmbedInfo Edge)
annotateGraph gr = ING.gmap edgeMapper gr
  where
    edgeMapper :: ING.Context SgNamedNode Edge
               -> ING.Context SgNamedNode (EmbedInfo Edge)
    edgeMapper (inEdges, node, nodeLabel, outEdges)
      = (getInEmbedInfo node inEdges
        , node
        , nodeLabel
        , getOutEmbedInfo node outEdges)
    getInEmbedInfo toNode
      = fmap (\(e, fromNode)
               -> (EmbedInfo (findEmbedDir gr fromNode toNode e) e, fromNode))
    getOutEmbedInfo fromNode
     = fmap (\(e, toNode)
              -> (EmbedInfo (findEmbedDir gr fromNode toNode e) e, toNode))

-- END annotateGraph --
-- START collapseAnnotatedGraph --

findEdgeLabel :: ING.Graph gr => gr a b -> ING.Node -> ING.Node -> Maybe b
findEdgeLabel graph node1 node2 = fmap fst matchingEdges where
  labelledEdges = ING.lneighbors graph node1
  matchingEdges = find ((== node2) . snd) labelledEdges

-- | Replace the a node's label
changeNodeLabel :: ING.DynGraph gr => ING.Node -> a -> gr a b -> gr a b
changeNodeLabel node newLabel graph = case ING.match node graph of
  (Just (inEdges, _, _, outEdges), restOfTheGraph)
    -> (inEdges, node, newLabel, outEdges) ING.& restOfTheGraph
  (Nothing, _) -> graph

-- TODO Wrap the SyntaxNodes in an Embedder type so that this function does not
-- require pattern matching.
addChildrenToNodeLabel :: [(SgNamedNode, Edge)] -> SyntaxNode -> SyntaxNode
addChildrenToNodeLabel children oldSyntaxNode = case oldSyntaxNode of
  ApplyNode flavor x existingNodes
    -> ApplyNode flavor x
       (children <> existingNodes)
  CaseOrMultiIfNode tag x existingNodes
    -> CaseOrMultiIfNode tag x
       (children <> existingNodes)
  FunctionDefNode labels existingNodes innerNodes
    -> FunctionDefNode
       labels
       (children <> existingNodes)
       innerNodes
  _ -> oldSyntaxNode

-- | Change the node label of the parent to be nested.
embedChildSyntaxNode :: ING.DynGraph gr =>
  ING.Node -> ING.Node -> AnnotatedGraph gr -> AnnotatedGraph gr
embedChildSyntaxNode parentNode childNode oldGraph = newGraph
  where
    mChildAndEdge =
      (,) <$> ING.lab oldGraph childNode
      <*> findEdgeLabel oldGraph parentNode childNode
    newGraph = case ING.lab oldGraph parentNode of
      Nothing -> error "embedChildSyntaxNode: parentNode not found"
      Just (NodeInfo isChild oldNodeLabel) ->
        -- TODO Refactor with the Maybe Monad?
        case mChildAndEdge of
          Nothing -> error "embedChildSyntaxNode: childNode not found."
          Just (NodeInfo _ childNodeLab, EmbedInfo _ edge)
            -> changeNodeLabel childNode (NodeInfo True childNodeLab)
               $ changeNodeLabel parentNode newNodeLabel oldGraph
            where
              SgNamedNode nodeName oldSyntaxNode = oldNodeLabel
              newSyntaxNode
                = addChildrenToNodeLabel [(childNodeLab, edge)] oldSyntaxNode
              newNodeLabel = NodeInfo isChild (SgNamedNode nodeName newSyntaxNode)

-- TODO This is buggy since it needs to transfer edges to the root ancestor, not
-- the immediate parent. Otherwise some edges will be between child nodes. Or
-- better yet, don't modify the graph edges, and change the bool in NodeInfo
-- to a Maybe Node which is the nodes parent. Use this info to find the root
-- ancestor when needed.
changeEdgeToParent :: ING.Node -> ING.Node -> ING.LEdge b -> ING.LEdge b
changeEdgeToParent parentNode childNode (fromNode, toNode, lab)
  = (toParent fromNode, toParent toNode, lab)
  where
    toParent node = if node == childNode then parentNode else node

collapseEdge :: (HasCallStack, ING.DynGraph gr)
             => AnnotatedGraph gr
             -> ING.LEdge (EmbedInfo Edge)
             -> AnnotatedGraph gr
collapseEdge oldGraph (fromNode, toNode, e@(EmbedInfo mEmbedDir _))
  = case mEmbedDir of
      Nothing -> oldGraph
      Just embedDir -> graphWithEdgesTransferred
        where
          (parentNode, childNode) = parentAndChild embedDir (fromNode, toNode)
          childEmbeddedGraph
            = embedChildSyntaxNode parentNode childNode oldGraph
          childEdgesToTransfer
            = changeEdgeToParent parentNode childNode
              <$> filter
              (\(_, _, edge) -> edge /= e)
              (ING.inn oldGraph childNode <> ING.out oldGraph childNode)
          graphWithEdgesTransferred
            = ING.insEdges childEdgesToTransfer childEmbeddedGraph


collapseAnnotatedGraph :: (HasCallStack, ING.DynGraph gr)
                       => gr SgNamedNode (EmbedInfo Edge)
                       -> AnnotatedGraph gr
collapseAnnotatedGraph origGraph = newGraph
  where
    defaultNodeInfoGraph = ING.nmap (NodeInfo False) origGraph
   -- TODO Check that there are no embedded edges left.
    newGraph = foldl' collapseEdge defaultNodeInfoGraph (ING.labEdges origGraph)
