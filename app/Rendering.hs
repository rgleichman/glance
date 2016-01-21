{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Rendering (
  Drawing(..),
  portToPort,
  iconToPort,
  iconToIcon,
  iconToIconEnds,
  iconHeadToPort,
  iconTailToPort,
  toNames,
  renderDrawing
) where

import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Diagrams.Backend.SVG(B)

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
--import Data.GraphViz.Commands
import qualified Data.Map as Map
import Data.Maybe(fromMaybe, isJust)
import qualified Debug.Trace
import Data.List(minimumBy)
import Data.Function(on)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Typeable(Typeable)

import Icons
import Types(Edge(..), Connection, Drawing(..), EdgeEnd(..))


-- | Convert a map of names and icons, to a list of names and diagrams.
-- The subDiagramMap
makeNamedMap :: IsName name => [(Name, Diagram B)] -> [(name, Icon)] -> [(name, Diagram B)]
makeNamedMap subDiagramMap =
  map (\(name, icon) -> (name, iconToDiagram icon subDiagramMap # nameDiagram name))

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (\(x, y) -> (f x, y))

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

noEnds = (EndNone, EndNone)

--portToPort :: (IsName a, IsName c) => a -> b -> c -> d -> Edge
portToPort :: (IsName a, IsName b) => a -> Int -> b -> Int -> Edge
portToPort a b c d = Edge (toName a, Just b, toName c, Just d) noEnds

iconToPort :: (IsName a, IsName b) => a -> b -> Int -> Edge
iconToPort a   c d = Edge (toName a, Nothing, toName c, Just d) noEnds

iconToIcon :: (IsName a, IsName b) => a -> b -> Edge
iconToIcon a   c   = Edge (toName a, Nothing, toName c, Nothing) noEnds


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: (IsName a, IsName b) => a -> EdgeEnd -> b -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge (toName a, Nothing, toName c, Nothing) (b, d)

iconHeadToPort a endHead c d = Edge (toName a, Nothing, toName c, Just d) (EndNone, endHead)

iconTailToPort a endTail c d = Edge (toName a, Nothing, toName c, Just d) (endTail, EndNone)

edgesToGraph :: (Ord v) => [v] -> [(v, t, v , t1)] -> Gr v ()
edgesToGraph names edges = mkGraph names simpleEdges
  where
    simpleEdges = map (\(a, _, c, _) -> (a, c, ())) edges

-- Custom arrow tail for the arg1 result circle.
-- The ArrowHT type does not seem to be documented.
arg1ResHT :: (RealFloat n) => ArrowHT n
arg1ResHT len _ = (circle (len / 2) # alignR, mempty)

getArrowOpts :: (RealFloat n, Typeable n) => (EdgeEnd, EdgeEnd) -> ArrowOpts n
getArrowOpts (t, h) = arrowOptions
  where
    lookupEnd :: (RealFloat n, Typeable n) => EdgeEnd -> ArrowOpts n -> ArrowOpts n
    lookupEnd EndNone = id
    lookupEnd EndAp1Arg = (arrowHead .~ thorn) . (headTexture .~ solid cyan)
    lookupEnd EndAp1Result = (arrowTail .~ arg1ResHT) . (tailTexture .~ solid cyan)
    arrowOptions =
      with & arrowHead .~ noHead
      & arrowTail .~ noTail
      & lengths .~ large
      & shaftStyle %~ lwG defaultLineWidth . lc white
      & (lookupEnd t) & (lookupEnd h)

plainLine = getArrowOpts (EndNone, EndNone)

connectMaybePorts :: Edge -> Diagram B -> Diagram B
connectMaybePorts (Edge (icon0, Just port0, icon1, Just port1) ends) =
  connect'
  (getArrowOpts ends)
  (icon0 .> port0)
  (icon1 .> port1)
connectMaybePorts (Edge (icon0, Nothing, icon1, Just port1) ends) =
  connectOutside' (getArrowOpts ends) icon0 (icon1 .> port1)
connectMaybePorts (Edge (icon0, Just port0, icon1, Nothing) ends) =
  connectOutside' (getArrowOpts ends) (icon0 .> port0) icon1
connectMaybePorts (Edge (icon0, Nothing, icon1, Nothing) ends) =
  connectOutside' (getArrowOpts ends) icon0 icon1

makeConnections :: [Edge] -> Diagram B -> Diagram B
makeConnections edges = applyAll connections
  where
    connections = map connectMaybePorts edges

-- | Returns [(myport, other node, other node's port)]
connectedPorts :: [Connection] -> Name -> [(Int, Name, Maybe Int)]
connectedPorts edges name = map edgeToPort $ filter nameInEdge edges
  where
    nameInEdge (n1, p1, n2, p2) = (name == n1 && isJust p1) || (name == n2 && isJust p2)
    edgeToPort (n1, p1, n2, p2) =
      if name == n1
        then (fromMaybe (error "connectedPorts port is Nothing") p1, n2, p2)
        else (fromMaybe (error "connectedPorts port is Nothing") p2, n1, p1)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "/n") a

totalLenghtOfLines :: Double -> P2 Double -> [(P2 Double, P2 Double)] -> Double
totalLenghtOfLines angle myLocation edges = sum $ map edgeDist edges
  where
    --edgeDist :: (P2 a, P2 a) -> Double
    edgeDist (relativePortLocation, iconLocation) =
      -- The squaring here is arbitrary. Distance should be replaced with angle diff.
      (norm $  absPortVec ^-^ iconLocationVec) ** 2
      where
        -- todo: is there a better way to convert from Points to vectors?
        relPortVec = r2 $ unp2 relativePortLocation
        iconLocationVec = r2 $ unp2 iconLocation
        myLocVec = r2 $ unp2 myLocation
        absPortVec = myLocVec ^+^ (rotateBy angle relPortVec)

-- | returns (angle, total distance)
angleWithMinDist :: P2 Double -> [(P2 Double, P2 Double)] -> (Double, Double)
angleWithMinDist myLocation edges =
  minimumBy (compare `on` snd) $ map totalLength [0,(1/40)..1]
  where
    totalLength angle = (angle, totalLenghtOfLines angle myLocation edges)

-- constant
scaleFactor = 0.02

getFromMapAndScale :: (Fractional a, Functor f, Ord k) => Map.Map k (f a) -> k -> f a
getFromMapAndScale posMap name = scaleFactor *^ (posMap Map.! name)

-- | rotateNodes rotates the nodes such that the distance of its connecting lines
-- are minimized.
-- Precondition: the diagrams are already centered
-- todo: confirm precondition (or use a newtype)
rotateNodes :: Map.Map Name (Point V2 Double) -> [(Name, Diagram B)] -> [Connection] -> [(Name, Diagram B)]
rotateNodes positionMap nameDiagramMap edges = map rotateDiagram nameDiagramMap
  where
    rotateDiagram (name, dia) = (name, diaToUse)
      where
        flippedDia = reflectX dia
        (unflippedAngle, unflippedDist) = minAngleForDia dia
        (flippedAngle, flippedDist) = minAngleForDia flippedDia
        diaToUse = if flippedDist < unflippedDist
          then rotateBy flippedAngle flippedDia
          else rotateBy unflippedAngle dia
        minAngleForDia :: Diagram B -> (Double, Double)
        minAngleForDia dia = minAngle where
        --ports = Debug.Trace.trace ((show $ names dia) ++ "\n") $ names dia
          ports = names dia
          namesOfPortsWithLines = connectedPorts edges name
          portEdges = map makePortEdge $ filter iconInMap namesOfPortsWithLines
          iconInMap (_, otherIconName, _) = Map.member otherIconName positionMap
          makePortEdge (portInt, otherIconName, _) = (getPortPoint portInt, getFromMapAndScale positionMap otherIconName)
          getPortPoint :: Int -> P2 Double
          getPortPoint x = head $ fromMaybe
            (error "port not found")
            (lookup (name .> x) ports)
          minAngle = angleWithMinDist (getFromMapAndScale positionMap name) portEdges

placeNodes layoutResult nameDiagramMap edges = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    rotatedNameDiagramMap = rotateNodes positionMap nameDiagramMap edges
    placedNodes = map placeNode rotatedNameDiagramMap
    --placedNodes = map placeNode nameDiagramMap
    -- todo: Not sure if the diagrams should already be centered at this point.
    placeNode (name, diagram) = place (diagram # centerXY) (scaleFactor *^ (positionMap Map.! name))

doGraphLayout :: Gr Name e -> [(Name, Diagram B)] -> (Diagram B -> r) -> [Connection] -> IO r
doGraphLayout graph nameDiagramMap connectNodes edges = do
  layoutResult <- layoutGraph' layoutParams GVA.Neato graph
  return $ placeNodes layoutResult nameDiagramMap edges # connectNodes
  where
    layoutParams :: GV.GraphvizParams Int v e () v
    layoutParams = GV.defaultParams{
      GV.globalAttributes =
        [ GV.NodeAttrs [GVA.Shape GVA.Circle]
        , GV.GraphAttrs [GVA.Overlap GVA.ScaleXYOverlaps, GVA.Splines GVA.LineEdges]
        ],
      GV.fmtEdge = const [GV.arrowTo GV.noArrow],
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, l) -> [GV.Attribute]
    nodeAttribute (nodeInt, _) =
      -- todo: Potential bug. GVA.Width and GVA.Height have a minimum of 0.01
      -- throw an error if the width or height are less than 0.01
      [GVA.Width shapeDimensions, GVA.Height shapeDimensions]
      where
        shapeDimensions = max (width dia) (height dia)
        --todo: Hack! Using (!!) here relies upon the implementation of Diagrams.TwoD.GraphViz.mkGraph
        -- to name the nodes in order
        (_, dia) = nameDiagramMap !! nodeInt

renderDrawing :: Drawing -> IO (Diagram B)
renderDrawing (Drawing nameIconMap edges subDrawings) = do
  subDiagramMap <- mapM subDrawingMapper subDrawings
  let diagramMap = makeNamedMap subDiagramMap nameIconMap
  --mapM_ (putStrLn . (++"\n") . show . (map fst) . names . snd) diagramMap
  doGraphLayout (edgesToGraph iconNames connections) diagramMap (makeConnections edges) connections
  where
    connections = map edgeConnection edges
    iconNames = map fst nameIconMap
    subDrawingMapper (name, subDrawing) = do
      subDiagram <- renderDrawing subDrawing
      return (name, subDiagram)
