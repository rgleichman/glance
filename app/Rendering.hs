{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Rendering (
  Drawing(..),
  portToPort,
  iconToPort,
  iconToIcon,
  toNames,
  renderDrawing
) where

import Diagrams.Prelude
import Diagrams.TwoD.GraphViz

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as GVA
import Data.GraphViz.Commands
import Data.Map((!))
import Data.Maybe (fromMaybe)

import Icons

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing b = Drawing [(Name, Icon)] b [(Name, Drawing b)]

makeNamedMap subDiagramMap =
  map (\(label, dia) -> (label, iconToDiagram dia subDiagramMap # nameDiagram label))

mapFst f = map (\(x, y) -> (f x, y))

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

portToPort a b c d = (toName a, Just $ PortName b, toName c, Just $ PortName d)
iconToPort a   c d = (toName a, Nothing, toName c, Just $ PortName d)
iconToIcon a   c   = (toName a, Nothing, toName c, Nothing)

edgesToGraph labels edges = mkGraph labels simpleEdges
  where
    simpleEdges = map (\(a, _, c, _) -> (a, c, ())) edges

uncurry4 f (a, b, c, d) = f a b c d

makeConnections edges = applyAll connections
  where
    connections = map (uncurry4 connectMaybePorts) edges

placeNodes scaleFactor layoutResult labelDiagramMap = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    placedNodes = map mapper labelDiagramMap
    mapper (label, diagram) = placedNode
      where
        --maybeDiagram = lookup label labelDiagramMap
        placedNode = place
          diagram
          --(fromMaybe (error ("placeNodes: label not in map: " ++ (show (map fst labelDiagramMap)))) maybeDiagram)
          (scaleFactor * positionMap ! label)

doGraphLayout scaleFactor graph labelDiagramMap connectNodes = do
  layoutResult <- layoutGraph Neato graph
  return $ placeNodes scaleFactor layoutResult labelDiagramMap # connectNodes

renderDrawing (Drawing nameIconMap edges subDrawings) scaleFactor = do
  subDiagramMap <- mapM subDrawingMapper subDrawings
  let diagramMap = makeNamedMap subDiagramMap nameIconMap
  doGraphLayout scaleFactor (edgesToGraph iconNames edges) diagramMap $ makeConnections edges
  where
    iconNames = map fst nameIconMap
    subDrawingMapper (label, subDrawing) = do
      subDiagram <- renderDrawing subDrawing (0.4 * scaleFactor)
      return (label, subDiagram)
