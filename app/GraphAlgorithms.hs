{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
module GraphAlgorithms(
  ParentType(..),
  annotateGraph,
  collapseAnnotatedGraph
  ) where

import Control.Arrow(first)
import qualified Data.Graph.Inductive as ING
import Data.List(foldl', find)
import Data.Tuple(swap)
import GHC.Stack(HasCallStack)

import Constants(pattern ResultPortConst, pattern InputPortConst)
import Types(SyntaxNode(..), IngSyntaxGraph, Edge(..),
             CaseOrMultiIfTag(..), Port(..), NameAndPort(..), SgNamedNode
            , AnnotatedGraph, EmbedInfo(..), EmbedDirection(..), NodeInfo(..)
            , Embedder(..), Named(..), EmbedderSyntaxNode)
import Util(fromMaybeError)

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

-- TODO Use pattern synonyms here
-- | A syntaxNodeIsEmbeddable if it can be collapsed into another node
syntaxNodeIsEmbeddable :: ParentType
                       -> SyntaxNode
                       -> Maybe Port
                       -> Maybe Port
                       -> Bool
syntaxNodeIsEmbeddable parentType syntaxNode mParentPort mChildPort
  = case (parentType, syntaxNode) of
      (ApplyParent, ApplyNode _ _) -> parentPortNotResult
      (ApplyParent, LiteralNode _) -> parentPortNotResult
      (ApplyParent, FunctionDefNode _ _)
        -> isInput mParentPort && isResult mChildPort

      -- The match below works, but can make messy drawings with the current
      -- icon for lambdas.
      -- (LambdaParent, ApplyNode _ _ _) -> parentPortIsInput
      (LambdaParent, LiteralNode _) -> parentPortIsInput
      (LambdaParent, FunctionDefNode _ _)
        -> parentPortIsInput && isResult mChildPort

      (CaseParent, LiteralNode _) -> parentPortNotResult
      (CaseParent, ApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput
      (CaseParent, PatternApplyNode _ _)
        -> parentPortNotResult && parentPortNotInput

      (MultiIfParent, LiteralNode _) -> parentPortNotResult
      (MultiIfParent, ApplyNode _ _)
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

    parentPortIsInput = isInput mParentPort

    parentPortNotInput = not $ isInput mParentPort
    parentPortNotResult = not $ isResult mParentPort

parentTypeForNode :: SyntaxNode -> ParentType
parentTypeForNode n = case n of
  (ApplyNode _ _) -> ApplyParent
  CaseOrMultiIfNode CaseTag _ _ -> CaseParent
  CaseOrMultiIfNode MultiIfTag _ _ -> MultiIfParent
  (FunctionDefNode _ _) -> LambdaParent
  _ -> NotAParent

lookupSyntaxNode :: ING.Graph gr =>
  IngSyntaxGraph gr -> ING.Node -> Maybe EmbedderSyntaxNode
lookupSyntaxNode gr node = naVal <$> ING.lab gr node

lookupParentType :: ING.Graph gr => IngSyntaxGraph gr -> ING.Node -> ParentType
lookupParentType graph node
  = maybe NotAParent parentTypeForNode $ emNode <$> lookupSyntaxNode graph node

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
        (emNode childSyntaxNode)
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

-- TODO Change CaseOrMultiIfNode to use Embedder, then simplify the
-- type of children.
addChildrenToNodeLabel ::
  [(SgNamedNode, Edge)] -> EmbedderSyntaxNode -> EmbedderSyntaxNode
addChildrenToNodeLabel children (Embedder existingNodes oldSyntaxNode)
  = case oldSyntaxNode of
      CaseOrMultiIfNode tag x caseExistingNodes
        -> Embedder [] $ CaseOrMultiIfNode tag x
           (children <> caseExistingNodes)
      _ -> Embedder (fmap (first naName) children <> existingNodes) oldSyntaxNode

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
            -> changeNodeLabel
               childNode
               (NodeInfo (Just parentNode) childNodeLab)
               $ changeNodeLabel parentNode newNodeLabel oldGraph
            where
              Named nodeName oldSyntaxNode = oldNodeLabel
              newSyntaxNode
                = addChildrenToNodeLabel [(childNodeLab, edge)] oldSyntaxNode
              newNodeLabel = NodeInfo isChild (Named nodeName newSyntaxNode)

collapseEdge :: (HasCallStack, ING.DynGraph gr)
             => AnnotatedGraph gr
             -> ING.LEdge (EmbedInfo Edge)
             -> AnnotatedGraph gr
collapseEdge oldGraph lEdge@(fromNode, toNode, EmbedInfo mEmbedDir _)
  = case mEmbedDir of
      Nothing -> oldGraph
      Just embedDir -> ING.delLEdge lEdge childEmbeddedGraph
        where
          (parentNode, childNode) = parentAndChild embedDir (fromNode, toNode)
          childEmbeddedGraph
            = embedChildSyntaxNode parentNode childNode oldGraph

mapEdges :: (ING.Graph gr1, ING.Graph gr2)
  => (ING.LEdge b1 -> ING.LEdge b2)
  -> gr1 a b1
  -> gr2 a b2
mapEdges f gr = ING.mkGraph nodes mappedEdges
  where
    nodes = ING.labNodes gr
    mappedEdges = f <$> ING.labEdges gr

findRootAncestor :: ING.Graph gr
  => gr (NodeInfo a) b -> ING.Node -> ING.Node
findRootAncestor gr node =
  let nodeLab = fromMaybeError
        "findRootAncestor: node does not exist"
        (ING.lab gr node)
  in
    case niParent nodeLab of
      Nothing -> node
      Just parentNode -> findRootAncestor gr parentNode

-- Note: modifying the edges could probably be eliminated if the algorithms in
-- Rendering were re-written to us the node's parent.
-- | For all of the graph edges, this function moves edge to from and to nodes
-- of the edge to be root (the parents's parent parent etc.) of the edge's
-- from and to nodes.
moveEdges :: (ING.Graph gr1, ING.Graph gr2)
  => gr1 (NodeInfo a) b -> gr2 (NodeInfo a) b
moveEdges gr = mapEdges moveEdge gr
  where
    moveEdge (fromNode, toNode, label) = (newFrom, newTo, label)
      where
        newFrom = findRootAncestor gr fromNode
        newTo = findRootAncestor gr toNode

collapseAnnotatedGraph :: (HasCallStack, ING.DynGraph gr)
                       => gr SgNamedNode (EmbedInfo Edge)
                       -> AnnotatedGraph gr
collapseAnnotatedGraph origGraph = moveEdges newGraph
  where
    defaultNodeInfoGraph = ING.nmap (NodeInfo Nothing) origGraph
   -- TODO Check that there are no embedded edges left.
    newGraph = foldl' collapseEdge defaultNodeInfoGraph (ING.labEdges origGraph)
