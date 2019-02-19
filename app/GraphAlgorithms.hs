{-# LANGUAGE MultiWayIf #-}
module GraphAlgorithms(
  ParentType(..),
  annotateGraph,
  collapseAnnotatedGraph
  ) where

import qualified Control.Arrow as Arrow
import qualified Data.Graph.Inductive as ING
import Data.List(foldl', find)
import Data.Tuple(swap)

import Types(SyntaxNode(..), IngSyntaxGraph, Edge(..),
             CaseOrMultiIfTag(..), Port(..), NameAndPort(..), SgNamedNode(..)
            , AnnotatedGraph, EmbedInfo(..), EmbedDirection(..))
import Util(sgNamedNodeToSyntaxNode)
{-# ANN module "HLint: ignore Use record patterns" #-}

data ParentType = ApplyParent
                | CaseParent
                | MultiIfParent
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
syntaxNodeIsEmbeddable parentType n mParentPort _mChildPort
  = case (parentType, n) of
      (ApplyParent, LikeApplyNode _ _) -> parentPortNotResult
      (ApplyParent, LiteralNode _) -> parentPortNotResult
      -- TODO Embedded FunctionDefNodes are missing their enclosures.
      -- (ApplyParent, FunctionDefNode _ _)
      --   -> isInput mParentPort && isResult mChildPort

      (CaseParent, LiteralNode _) -> parentPortNotResult
      (CaseParent, LikeApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput
      (CaseParent, NestedPatternApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput

      (MultiIfParent, LiteralNode _) -> parentPortNotResult
      (MultiIfParent, LikeApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput

      _ -> False
  where
    isInput mPort = case mPort of
      Just (Port 0) -> True
      _ -> False

    isResult mPort = case mPort of
      Just (Port 1) -> True
      Just _ -> False
      _ -> True

    parentPortNotInput = not $ isInput mParentPort
    parentPortNotResult = not $ isResult mParentPort

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode n = case n of
  LikeApplyNode _ _ -> ApplyParent
  NestedApplyNode _ _ _ -> ApplyParent
  CaseNode _ -> CaseParent
  MultiIfNode _ -> MultiIfParent
  NestedCaseOrMultiIfNode CaseTag _ _ -> CaseParent
  NestedCaseOrMultiIfNode MultiIfTag _ _ -> MultiIfParent
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

annotateGraph :: ING.DynGraph gr => IngSyntaxGraph gr -> AnnotatedGraph gr
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
changeNodeLabel :: ING.DynGraph gr => gr a b -> ING.Node -> a -> gr a b
changeNodeLabel graph node newLabel = case ING.match node graph of
  (Just (inEdges, _, _, outEdges), restOfTheGraph)
    -> (inEdges, node, newLabel, outEdges) ING.& restOfTheGraph
  (Nothing, _) -> graph

-- | Change the node label of the parent to be nested.
embedChildSyntaxNode :: ING.DynGraph gr =>
  ING.Node -> ING.Node -> AnnotatedGraph gr -> AnnotatedGraph gr
embedChildSyntaxNode parentNode childNode oldGraph = newGraph
  where
    mChildAndEdge =
      (,) <$> ING.lab oldGraph childNode
      <*> findEdgeLabel oldGraph parentNode childNode
    childrenAndEdgesToParent = case mChildAndEdge of
      Nothing -> []
      Just childAndEdge -> [Arrow.second eiVal childAndEdge]
    newGraph = case ING.lab oldGraph parentNode of
      Nothing -> oldGraph
      Just oldNodeLabel -> changeNodeLabel oldGraph parentNode newNodeLabel
        where
          SgNamedNode nodeName oldSyntaxNode = oldNodeLabel
          newNodeLabel = SgNamedNode nodeName newSyntaxNode
          newSyntaxNode = case oldSyntaxNode of
            LikeApplyNode flavor x
              -> NestedApplyNode flavor x childrenAndEdgesToParent
            NestedApplyNode flavor x existingNodes
              -> NestedApplyNode flavor x
                 (childrenAndEdgesToParent <> existingNodes)
            CaseNode x
              -> NestedCaseOrMultiIfNode CaseTag x childrenAndEdgesToParent
            NestedCaseOrMultiIfNode tag x existingNodes
              -> NestedCaseOrMultiIfNode tag x
                 (childrenAndEdgesToParent <> existingNodes)
            MultiIfNode x
              -> NestedCaseOrMultiIfNode MultiIfTag x childrenAndEdgesToParent
            _ -> oldSyntaxNode

changeEdgeToParent :: ING.Node -> ING.Node -> ING.LEdge b -> ING.LEdge b
changeEdgeToParent parentNode childNode (fromNode, toNode, lab)
  = (toParent fromNode, toParent toNode, lab)
  where
    toParent node = if node == childNode then parentNode else node

collapseEdge :: ING.DynGraph gr
             => AnnotatedGraph gr
             -> ING.LEdge (EmbedInfo Edge)
             -> AnnotatedGraph gr
collapseEdge oldGraph (fromNode, toNode, e@(EmbedInfo mEmbedDir _))
  = case mEmbedDir of
      Nothing -> oldGraph
      Just embedDir -> childDeletedGraph
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
          childDeletedGraph = ING.delNode childNode graphWithEdgesTransferred


collapseAnnotatedGraph :: ING.DynGraph gr
                       => AnnotatedGraph gr
                       -> AnnotatedGraph gr
collapseAnnotatedGraph origGraph = newGraph
  where
   -- TODO Check that there are no embedded edges left.
    newGraph = foldl' collapseEdge origGraph (ING.labEdges origGraph)
