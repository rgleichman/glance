{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures #-}

module Rendering (
  renderDrawing
) where

import Diagrams.Prelude
import Diagrams.TwoD.GraphViz(mkGraph, getGraph, layoutGraph')
import Diagrams.TwoD.Text(Text)
--import Diagrams.Backend.SVG(B)

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands
import qualified Data.Map as Map
import Data.Maybe(isJust)
--import qualified Debug.Trace
import Data.List(minimumBy)
import Data.Function(on)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Typeable(Typeable)
--import Data.Word(Word16)

import Icons(colorScheme, Icon(..), iconToDiagram, nameDiagram, defaultLineWidth, ColorStyle(..))
import Types(Edge(..), EdgeOption(..), Connection, Drawing(..), EdgeEnd(..), NameAndPort(..))
import Util(fromMaybeError)

-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

-- CONSTANT
scaleFactor :: (Fractional a) => a

-- For Neato
scaleFactor = 0.12

-- For Fdp
--scaleFactor = 0.09

--scaleFactor = 0.04

drawingToGraphvizScaleFactor :: Double
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08

-- For Neato, PrismOverlap
drawingToGraphvizScaleFactor = 0.15

-- CONVERTING Edges AND Icons TO DIAGRAMS --

-- | Convert a map of names and icons, to a list of names and diagrams.
-- The first argument is the subdiagram map used for the inside of lambdaIcons
-- The second argument is the map of icons that should be converted to diagrams.
--makeNamedMap :: IsName name => [(Name, Diagram B)] -> [(name, Icon)] -> [(name, Diagram B)]

--
-- makeNamedMap ::
--    (RealFloat n, Typeable n, Renderable (Path V2 n) b,
--       Renderable (Diagrams.TwoD.Text.Text n) b, IsName nm) =>
--      [(Name, QDiagram b V2 n Any)]-> [(nm, Icon)] -> [(nm, QDiagram b V2 n Any)]
makeNamedMap subDiagramMap =
  map (\(name, icon) -> (name, iconToDiagram icon subDiagramMap))

-- | Make an inductive Graph from a list of node names, and a list of Connections.
edgesToGraph :: [Name] -> [(NameAndPort, NameAndPort)] -> Gr Name ()
edgesToGraph iconNames edges = mkGraph iconNames simpleEdges
  where
    simpleEdges = map (\(NameAndPort a _, NameAndPort c _) -> (a, c, ())) edges

-- | Custom arrow tail for the arg1 result circle.
-- The ArrowHT type does not seem to be documented.
arg1ResT :: (RealFloat n) => ArrowHT n
arg1ResT len _ = (circle (len / 2) # alignR, mempty)

-- | Arrow head version of arg1ResT
arg1ResH :: (RealFloat n) => ArrowHT n
arg1ResH len _ = (circle (len / 2) # alignL, mempty)

getArrowOpts :: (RealFloat n, Typeable n) => (EdgeEnd, EdgeEnd) -> [EdgeOption]-> ArrowOpts n
getArrowOpts (t, h) opts = arrowOptions
  where
    shaftColor = if EdgeInPattern `elem` opts then patternC else lineC
    ap1ArgTexture = solid (backgroundC colorScheme)
    ap1ArgStyle = lwG defaultLineWidth . lc (apply1C colorScheme)
    ap1ResultTexture = solid (apply1C colorScheme)

    lookupTail EndNone = id
    lookupTail EndAp1Arg = (arrowTail .~ dart')
      . (tailTexture .~ ap1ArgTexture) . (tailStyle %~  ap1ArgStyle)
    lookupTail EndAp1Result = (arrowTail .~ arg1ResT) . (tailTexture .~ ap1ResultTexture)

    lookupHead EndNone = id
    lookupHead EndAp1Arg = (arrowHead .~ dart)
      . (headTexture .~ ap1ArgTexture) . (headStyle %~ ap1ArgStyle)
    lookupHead EndAp1Result = (arrowHead .~ arg1ResH) . (headTexture .~ ap1ResultTexture)

    arrowOptions =
      with & arrowHead .~ noHead
      & arrowTail .~ noTail
      & lengths .~ global 0.75
      -- this parenthesis "%~ (lwG .. colorScheme))" is necessary for haskell-src-exts to parse the file.
      & shaftStyle %~ (lwG defaultLineWidth . lc (shaftColor colorScheme))
      & lookupTail t & lookupHead h

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts ::
   (RealFloat n, Typeable n, Renderable (Path V2 n) b) =>
     Edge -> QDiagram b V2 n Any -> QDiagram b V2 n Any
connectMaybePorts (Edge opts ends (NameAndPort icon0 (Just port0), NameAndPort icon1 (Just port1))) =
  connect'
  (getArrowOpts ends opts)
  (icon0 .> port0)
  (icon1 .> port1)
connectMaybePorts (Edge opts ends (NameAndPort icon0 Nothing, NameAndPort icon1 (Just port1))) =
  connectOutside' (getArrowOpts ends opts) icon0 (icon1 .> port1)
connectMaybePorts (Edge opts ends (NameAndPort icon0 (Just port0), NameAndPort icon1 Nothing)) =
  connectOutside' (getArrowOpts ends opts) (icon0 .> port0) icon1
connectMaybePorts (Edge opts ends (NameAndPort icon0 Nothing, NameAndPort icon1 Nothing)) =
  connectOutside' (getArrowOpts ends opts) icon0 icon1

makeConnections ::
   (RealFloat n, Typeable n, Renderable (Path V2 n) b) =>
     [Edge] -> QDiagram b V2 n Any -> QDiagram b V2 n Any
makeConnections edges = applyAll connections
  where
    connections = map connectMaybePorts edges

-- ROTATING/FLIPPING ICONS --

--printSelf :: (Show a) => a -> a
--printSelf a = Debug.Trace.trace (show a ++ "/n") a

{-# ANN totalLenghtOfLines "HLint: ignore Redundant bracket" #-}
{-# ANN totalLenghtOfLines "HLint: ignore Move brackets to avoid $" #-}
-- | For a specific icon, given its angle, location, and a list of pairs of locations
-- of (this icon's port, icon that connects to this port), return the sum of the
-- distances (possibly squared) between the ports and the icons they connect to.
-- This function is used to find that angle that minimizes the sum of distances.
totalLenghtOfLines :: Double -> P2 Double -> [(P2 Double, P2 Double)] -> Double
totalLenghtOfLines angle myLocation edges = sum $ map edgeDist edges
  where
    edgeDist :: (P2 Double, P2 Double) -> Double
    edgeDist (relativePortLocation, iconLocation) =
      -- The squaring here is arbitrary. Distance should be replaced with angle diff.
      (norm $  absPortVec ^-^ iconLocationVec) ** 2
      where
        P relPortVec = relativePortLocation
        P iconLocationVec = iconLocation
        P myLocVec = myLocation
        absPortVec = myLocVec ^+^ (rotateBy angle relPortVec)

 -- | For a specific icon, given its location, and a list of pairs of locations
-- of (this icon's port, icon that connects to this port), find the angle that
-- minimizes the the sum of the distances (possibly squared) between the ports
-- and the icons they connect to. Returns (angle, sum of distances).
-- todo: Return 0 immediatly if edges == [].
angleWithMinDist :: P2 Double -> [(P2 Double, P2 Double)] -> (Double, Double)
angleWithMinDist myLocation edges =
  minimumBy (compare `on` snd) $ map totalLength [0,(1/40)..1]
  where
    totalLength angle = (angle, totalLenghtOfLines angle myLocation edges)

getFromMapAndScale :: (Fractional a, Functor f, Ord k) => Map.Map k (f a) -> k -> f a
getFromMapAndScale posMap name = scaleFactor *^ (posMap Map.! name)

-- | Returns [(myport, other node, maybe other node's port)]
connectedPorts :: [Connection] -> Name -> [(Int, Name, Maybe Int)]
connectedPorts edges name = map edgeToPort $ filter nameInEdge edges
  where
    isPort = isJust
    nameInEdge (NameAndPort name1 port1, NameAndPort name2 port2) = (name == name1 && isPort port1) || (name == name2 && isPort port2)
    edgeToPort (NameAndPort name1 port1, NameAndPort name2 port2) =
      if name == name1
        then (fromMaybeError "connectedPorts: port is Nothing" port1, name2, port2)
        else (fromMaybeError "connectedPorts: port is Nothing" port2, name1, port1)

-- | rotateNodes rotates the nodes such that the distance of its connecting lines
-- are minimized.
-- Precondition: the diagrams are already centered
-- todo: confirm precondition (or use a newtype)
rotateNodes ::
   Semigroup m =>
     Map.Map Name (Point V2 Double)
     -> [(Name, Bool -> Double -> QDiagram b V2 Double m)]
     -> [Connection]
     -> [(Name, QDiagram b V2 Double m)]
rotateNodes positionMap nameDiagramMap edges = map rotateDiagram nameDiagramMap
  where
    rotateDiagram (name, originalDia) = (name, nameDiagram name transformedDia)
      where
        transformedDia = if flippedDist < unflippedDist
          then rotateBy flippedAngle . reflectX $ originalDia True flippedAngle
          else rotateBy unflippedAngle $ originalDia False unflippedAngle
        (unflippedAngle, unflippedDist) = minAngleForDia (originalDia False 0)
        (flippedAngle, flippedDist) = minAngleForDia (reflectX $ originalDia True 0)
        --minAngleForDia :: QDiagram b V2 Double m -> (Double, Double)
        minAngleForDia dia = minAngle where
        --ports = Debug.Trace.trace ((show $ names dia) ++ "\n") $ names dia
          ports = names dia
          namesOfPortsWithLines = connectedPorts edges name

          iconInMap :: (Int, Name, Maybe Int) -> Bool
          iconInMap (_, otherIconName, _) = Map.member otherIconName positionMap

          getPortPoint :: Int -> P2 Double
          getPortPoint x =
            -- TODO remove partial function head.
            head $ fromMaybeError
              ("rotateNodes: port not found. Port: " ++ show x ++ ". Valid ports: " ++ show ports)
              (lookup (toName x) ports)

          makePortEdge :: (Int, Name, Maybe Int) -> (P2 Double, P2 Double)
          makePortEdge (portInt, otherIconName, _) =
            (getPortPoint portInt, getFromMapAndScale positionMap otherIconName)

          portEdges = map makePortEdge $ filter iconInMap namesOfPortsWithLines

          minAngle = angleWithMinDist (getFromMapAndScale positionMap name) portEdges

type LayoutResult a = Gr (GV.AttributeNode Name) (GV.AttributeNode a)
placeNodes ::
   (Monoid m, Semigroup m) =>
     LayoutResult a
     -> [(Name, Bool -> Double -> QDiagram b V2 Double m)]
     -> [Connection]
     -> QDiagram b V2 Double m
placeNodes layoutResult nameDiagramMap edges = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    rotatedNameDiagramMap = rotateNodes positionMap nameDiagramMap edges
    placedNodes = map placeNode rotatedNameDiagramMap
    --placedNodes = map placeNode nameDiagramMap
    -- todo: Not sure if the diagrams should already be centered at this point.
    placeNode (name, diagram) = place (diagram # centerXY) (scaleFactor *^ (positionMap Map.! name))

doGraphLayout ::
   _ =>
     Gr Name e
     -> [(Name, Bool -> Double -> QDiagram b V2 Double m)]
     -> [Connection]
     -> IO (QDiagram b V2 Double m)
doGraphLayout graph nameDiagramMap edges = do
  layoutResult <- layoutGraph' layoutParams GVA.Neato graph
  --  layoutResult <- layoutGraph' layoutParams GVA.Fdp graph
  return $ placeNodes layoutResult nameDiagramMap edges
  where
    layoutParams :: GV.GraphvizParams Int v e () v
    --layoutParams :: GV.GraphvizParams Int l el Int l
    layoutParams = GV.defaultParams{
      GV.globalAttributes = [
        GV.NodeAttrs [GVA.Shape GVA.BoxShape]
        --GV.NodeAttrs [GVA.Shape GVA.Circle]
        , GV.GraphAttrs
          [
          --GVA.Overlap GVA.KeepOverlaps,
          --GVA.Overlap GVA.ScaleOverlaps,
          GVA.Overlap $ GVA.PrismOverlap (Just 1000),
          GVA.Splines GVA.LineEdges,
          GVA.OverlapScaling 4,
          GVA.OverlapShrink True
          ]
        ],
      GV.fmtEdge = const [GV.arrowTo GV.noArrow],
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, l) -> [GV.Attribute]
    nodeAttribute (nodeInt, _) =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width circleDiameter, GVA.Height circleDiameter]
      where
        --todo: Hack! Using (!!) here relies upon the implementation of Diagrams.TwoD.GraphViz.mkGraph
        -- to name the nodes in order
        (_, unTransformedDia) = (nameDiagramMap !! nodeInt)
        dia = unTransformedDia False 0

        diaWidth = drawingToGraphvizScaleFactor * (width dia)
        diaHeight = drawingToGraphvizScaleFactor * (height dia)
        circleDiameter' = max diaWidth diaHeight
        circleDiameter = if circleDiameter' <= 0.01 then error ("circleDiameter too small: " ++ show circleDiameter') else circleDiameter'


-- | Given a Drawing, produce a Diagram complete with rotated/flipped icons and
-- lines connecting ports and icons. IO is needed for the GraphViz layout.
renderDrawing ::
  _ =>
  Drawing -> IO (QDiagram b V2 Double Any)
renderDrawing (Drawing nameIconMap edges subDrawings) = do
  subDiagramMap <- traverse renderSubDrawing subDrawings
  let diagramMap = makeNamedMap subDiagramMap nameIconMap
  --mapM_ (putStrLn . (++"\n") . show . (map fst) . names . snd) diagramMap
  makeConnections edges <$>
    doGraphLayout (edgesToGraph iconNames connections) diagramMap connections
  where
    connections = map edgeConnection edges
    iconNames = map fst nameIconMap
    renderSubDrawing (name, subDrawing) = do
      subDiagram <- renderDrawing subDrawing
      return (name, subDiagram)
