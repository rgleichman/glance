{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing,
  customLayoutParams,
  renderIngSyntaxGraph
) where

import Diagrams.Core.Names(Name(..))
import Diagrams.Prelude hiding ((#), (&))
import Diagrams.TwoD.GraphViz(mkGraph, getGraph, layoutGraph')
--import Diagrams.Backend.SVG(B)

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Map as Map
import Data.Maybe(isJust)

import Control.Arrow(second)
import Data.Function(on)
import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List(minimumBy)
import Data.Typeable(Typeable)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands
--import qualified Debug.Trace
--import Data.Word(Word16)

import Icons(colorScheme, iconToDiagram, defaultLineWidth, ColorStyle(..), getPortAngles)
import TranslateCore(nodeToIcon)
import Types(Edge(..), Icon, EdgeOption(..), Connection, Drawing(..), EdgeEnd(..),
  NameAndPort(..), SpecialQDiagram, SpecialBackend, SyntaxNode, SpecialNum)
import Util(fromMaybeError)

-- If the inferred types for these functions becomes unweildy,
-- try using PartialTypeSignitures.

-- CONSTANT
graphvizScaleFactor :: (Fractional a) => a

-- For Neato
graphvizScaleFactor = 0.12

-- For Fdp
--scaleFactor = 0.09

--scaleFactor = 0.04

drawingToGraphvizScaleFactor :: Fractional a => a
-- For Neato, ScaleOverlaps
--drawingToGraphvizScaleFactor = 0.08

-- For Neato, PrismOverlap
drawingToGraphvizScaleFactor = 0.15

-- Note that the name type alias is different from the Name constructor.
getTopLevelName :: Name -> Name
getTopLevelName (Name []) = Name []
getTopLevelName (Name (x:_)) = Name [x]

-- TODO Refactor with syntaxGraphToFglGraph in TranslateCore
-- TODO Make this work with nested icons now that names are not qualified.
drawingToIconGraph :: Drawing -> Gr (Name, Icon) Edge
drawingToIconGraph (Drawing nodes edges) =
  mkGraph nodes labeledEdges where
    labeledEdges = fmap makeLabeledEdge edges
    makeLabeledEdge e@(Edge _ _ (NameAndPort n1 _, NameAndPort n2 _)) =
      let name1 = getTopLevelName n1
          name2 = getTopLevelName n2
      in
      ((name1, lookupInNodes name1), (name2, lookupInNodes name2), e) where
        lookupInNodes name = fromMaybeError errorString (lookup name nodes) where
          errorString =
            "syntaxGraphToFglGraph edge connects to non-existent node. Node Name ="
            ++ show name ++ " Edge=" ++ show e


-- | Custom arrow tail for the arg1 result circle.
-- The ArrowHT type does not seem to be documented.
arg1ResT :: (RealFloat n) => ArrowHT n
arg1ResT len _ = (alignR $ circle (len / 2), mempty)

-- | Arrow head version of arg1ResT
arg1ResH :: (RealFloat n) => ArrowHT n
arg1ResH len _ = (alignL $ circle (len / 2), mempty)

bezierShaft :: (V t ~ V2, TrailLike t) => Angle (N t) -> Angle (N t) -> t
bezierShaft angle1 angle2 = fromSegments [bezier3 c1 c2 x] where
  scaleFactor = 0.5
  x = r2 (1,0)
  c1 = rotate angle1 (scale scaleFactor unitX)
  c2 = rotate angle2 (scale scaleFactor unitX) ^+^ x

getArrowOpts :: (RealFloat n, Typeable n) => (EdgeEnd, EdgeEnd) -> [EdgeOption] -> (Angle n, Angle n) -> ArrowOpts n
getArrowOpts (t, h) opts (fromAngle, toAngle) = arrowOptions
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
      arrowHead .~ noHead $
      arrowTail .~ noTail $
      arrowShaft .~ bezierShaft fromAngle toAngle $
      lengths .~ global 0.75 $
      shaftStyle %~ (lwG defaultLineWidth . lcA (withOpacity (shaftColor colorScheme) 0.7)) $
      lookupHead h $ lookupTail t with

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: SpecialBackend b n =>
  (Angle n, Angle n)-> Edge -> SpecialQDiagram b n -> SpecialQDiagram b n
connectMaybePorts portAngles (Edge opts ends (NameAndPort name0 mPort1, NameAndPort name1 mPort2)) =
  connectFunc (getArrowOpts ends opts portAngles) qPort0 qPort1 where
  (connectFunc, qPort0, qPort1) = case (mPort1, mPort2) of
    (Just port0, Just port1) -> (connect', name0 .> port0, name1 .> port1)
    (Nothing, Just port1) -> (connectOutside', name0, name1 .> port1)
    (Just port0, Nothing) -> (connectOutside', name0 .> port0, name1)
    (_, _) -> (connectOutside', name0, name1)

-- START addEdges --
nameAndPortToName :: NameAndPort -> Name
nameAndPortToName (NameAndPort name mPort) = case mPort of
  Nothing -> name
  Just port -> name .> port

findPortAngles :: Floating n => (Name, Icon) -> NameAndPort -> [Angle n]
findPortAngles (nodeName, nodeIcon) (NameAndPort diaName mPort) = case mPort of
  Nothing -> []
  Just port -> foundAngles where
    mName = if nodeName == diaName then Nothing else Just diaName
    foundAngles = getPortAngles nodeIcon port mName

-- TODO Clean up the Angle arithmatic
pickClosestAngle :: SpecialNum n => (Bool, Angle n) -> Angle n -> Angle n -> Angle n -> [Angle n] -> Angle n
pickClosestAngle (nodeFlip, nodeAngle) emptyCase target shaftAngle angles = case angles of
  [] -> emptyCase
  _ -> (-) <$>
    fst (minimumBy (compare `on` snd) $ fmap angleDiff adjustedAngles)
    <*>
    shaftAngle
    where
      adjustedAngles = fmap adjustAngle angles
      angleDiff angle = (angle, angleBetween (angleV target) (angleV angle))

      adjustAngle angle = if nodeFlip then
        signedAngleBetween (rotate nodeAngle $ reflectX (angleV angle)) unitX
        else
        (+) <$> angle <*> nodeAngle


lookupNodeAngle ::  Show n => [((Name, Icon), (Bool, Angle n))] -> (Name, Icon) -> (Bool, Angle n)
lookupNodeAngle rotationMap key =
  fromMaybeError ("nodeVector: key not in rotaionMap. key = " ++ show key ++ "\n\n rotationMap = " ++ show rotationMap)
  $ lookup key rotationMap


makeEdge :: (SpecialBackend b n, ING.Graph gr) =>
  gr (Name, Icon) Edge -> SpecialQDiagram b n -> [((Name, Icon), (Bool, Angle n))] ->
  ING.LEdge Edge -> SpecialQDiagram b n -> SpecialQDiagram b n
makeEdge graph dia rotationMap (node0, node1, edge@(Edge _ _ (namePort0, namePort1))) =
  connectMaybePorts portAngles edge
  where
    node0label = fromMaybeError ("node0 is not in graph. node0: " ++ show node0) $
      ING.lab graph node0
    node1label = fromMaybeError ("node0 is not in graph. node1: " ++ show node1) $
      ING.lab graph node1

    node0Angle = lookupNodeAngle rotationMap node0label
    node1Angle = lookupNodeAngle rotationMap node1label

    diaNamePointMap = names dia
    port0Point = getPortPoint $ nameAndPortToName namePort0
    port1Point = getPortPoint $ nameAndPortToName namePort1
    shaftVector = port1Point .-. port0Point
    shaftAngle = signedAngleBetween shaftVector unitX

    icon0PortAngle = pickClosestAngle node0Angle mempty shaftAngle shaftAngle $ findPortAngles node0label namePort0

    shaftAnglePlusOneHalf = (+) <$> shaftAngle <*> (1/2 @@ turn)
    icon1PortAngle = pickClosestAngle node1Angle (1/2 @@ turn) shaftAnglePlusOneHalf shaftAngle $ findPortAngles node1label namePort1

    getPortPoint n = head $ fromMaybeError
      ("makeEdge: port not found. Port: " ++ show n ++ ". Valid ports: " ++ show diaNamePointMap)
      (lookup n diaNamePointMap)
    
    portAngles = (icon0PortAngle, icon1PortAngle)


addEdges :: (SpecialBackend b n, ING.Graph gr) =>
  gr (Name, Icon) Edge -> (SpecialQDiagram b n, [((Name, Icon), (Bool, Angle n))]) -> SpecialQDiagram b n
addEdges graph (dia, rotationMap) = applyAll connections dia
  where
    connections = makeEdge graph dia rotationMap <$> ING.labEdges graph

-- ROTATING/FLIPPING ICONS --

--printSelf :: (Show a) => a -> a
--printSelf a = Debug.Trace.trace (show a ++ "/n") a

{-# ANN totalLenghtOfLines "HLint: ignore Redundant bracket" #-}
{-# ANN totalLenghtOfLines "HLint: ignore Move brackets to avoid $" #-}
-- | For a specific icon, given its angle, location, and a list of pairs of locations
-- of (this icon's port, icon that connects to this port), return the sum of the
-- distances (possibly squared) between the ports and the icons they connect to.
-- This function is used to find that angle that minimizes the sum of distances.
totalLenghtOfLines :: Floating a => a -> P2 a -> [(P2 a, P2 a)] -> a
totalLenghtOfLines angle myLocation edges = sum $ map edgeDist edges
  where
    --edgeDist :: (P2 Double, P2 Double) -> Double
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
angleWithMinDist :: SpecialNum a => P2 a -> [(P2 a, P2 a)] -> (a, a)
angleWithMinDist myLocation edges =
  minimumBy (compare `on` snd) $ map totalLength [0,(1/12)..1]
  where
    totalLength angle = (angle, totalLenghtOfLines angle myLocation edges)

getFromMapAndScale :: (Fractional a, Functor f, Ord k) => Map.Map k (f a) -> k -> f a
getFromMapAndScale posMap name = graphvizScaleFactor *^ (posMap Map.! name)

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
rotateNodes :: SpecialBackend b n =>
  Map.Map (Name, Icon) (Point V2 n)
  -> [Connection]
  -> [((Name, Icon), SpecialQDiagram b n, (Bool, Angle n))]
rotateNodes positionMap edges = map rotateDiagram (Map.keys positionMap)
  where
    positionMapNameKeys = Map.mapKeys fst positionMap
    rotateDiagram key@(name, icon) = (key, transformedDia, (reflected, angle))
      where
        originalDia = iconToDiagram icon name
        reflected = flippedDist < unflippedDist
        angle = (if reflected then flippedAngle else unflippedAngle) @@ turn
        internallTransformedDia = originalDia reflected angle
        transformedDia = rotate angle $ (if reflected then reflectX else id) internallTransformedDia

        (unflippedAngle, unflippedDist) = minAngleForDia (originalDia False mempty)
        (flippedAngle, flippedDist) = minAngleForDia (reflectX $ originalDia True mempty)
        --minAngleForDia :: QDiagram b V2 Double m -> (Double, Double)
        minAngleForDia dia = minAngle where
        --ports = Debug.Trace.trace ((show $ names dia) ++ "\n") $ names dia
          ports = names dia
          namesOfPortsWithLines = connectedPorts edges name

          --iconInMap :: (Int, Name, Maybe Int) -> Bool
          iconInMap (_, otherIconName, _) = Map.member otherIconName positionMapNameKeys

          --getPortPoint :: Int -> P2 Double
          getPortPoint x =
            -- TODO remove partial function head.
            head $ fromMaybeError
              ("rotateNodes: port not found. Port: " ++ show x ++ ". Valid ports: " ++ show ports)
              (lookup (name .> toName x) ports)

          --makePortEdge :: (Int, Name, Maybe Int) -> (P2 Double, P2 Double)
          makePortEdge (portInt, otherIconName, _) =
            (getPortPoint portInt, getFromMapAndScale positionMapNameKeys otherIconName)

          portEdges = map makePortEdge $ filter iconInMap namesOfPortsWithLines

          minAngle = angleWithMinDist (getFromMapAndScale positionMapNameKeys name) portEdges


type LayoutResult a b = Gr (GV.AttributeNode (Name, b)) (GV.AttributeNode a)

placeNodes :: forall a b. SpecialBackend b Double =>
   LayoutResult a Icon
   -> [Edge]
   -> (SpecialQDiagram b Double, [((Name, Icon), (Bool, Angle Double))])
placeNodes layoutResult edges = (mconcat placedNodes, rotationMap)
  where
    connections = fmap edgeConnection edges
    positionMap = fst $ getGraph layoutResult
    -- The type annotation for rotatedNameDiagramMap is necessary here
    rotatedNameDiagramMap = rotateNodes positionMap connections :: [((Name, Icon), SpecialQDiagram b Double, (Bool, Angle Double))]
    rotationMap = fmap (\(x, _, z) -> (x, z)) rotatedNameDiagramMap

    placedNodes = map placeNode rotatedNameDiagramMap
    --placedNodes = map placeNode $ (\key@(name, icon) -> (key, iconToDiagram icon name False 0)) <$> Map.keys positionMap
    -- todo: Not sure if the diagrams should already be centered at this point.
    placeNode (key, diagram, _) = place (centerXY diagram) (graphvizScaleFactor *^ (positionMap Map.! key))

customLayoutParams :: GV.GraphvizParams n v e () v
customLayoutParams = GV.defaultParams{
  GV.globalAttributes = [
    GV.NodeAttrs [GVA.Shape GVA.BoxShape]
    --GV.NodeAttrs [GVA.Shape GVA.Circle]
    , GV.GraphAttrs
      [
      --GVA.Overlap GVA.KeepOverlaps,
      --GVA.Overlap GVA.ScaleOverlaps,
      GVA.Overlap $ GVA.PrismOverlap (Just 5000),
      GVA.Splines GVA.LineEdges,
      GVA.OverlapScaling 8,
      --GVA.OverlapScaling 4,
      GVA.OverlapShrink True
      ]
    ],
  GV.fmtEdge = const [GV.arrowTo GV.noArrow]
  }

doGraphLayout :: forall b.
  SpecialBackend b Double =>
  Gr (Name, Icon) Edge
  -> [Edge]
  -> IO (SpecialQDiagram b Double)
doGraphLayout graph edges = do
  layoutResult <- layoutGraph' layoutParams GVA.Neato graph
  --  layoutResult <- layoutGraph' layoutParams GVA.Fdp graph
  pure $ addEdges graph $ placeNodes layoutResult edges
  where
    layoutParams :: GV.GraphvizParams Int (Name,Icon) e () (Name,Icon)
    --layoutParams :: GV.GraphvizParams Int l el Int l
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, (Name, Icon)) -> [GV.Attribute]
    nodeAttribute (_, (_, nodeIcon)) =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width circleDiameter, GVA.Height circleDiameter]
      where
        -- This type annotation (:: SpecialQDiagram b n) requires Scoped Typed Variables, which only works if the function's
        -- type signiture has "forall b e."
        dia = iconToDiagram nodeIcon (toName "") False mempty :: SpecialQDiagram b Double

        diaWidth = drawingToGraphvizScaleFactor * width dia
        diaHeight = drawingToGraphvizScaleFactor * height dia
        circleDiameter' = max diaWidth diaHeight
        circleDiameter = if circleDiameter' <= 0.01 then error ("circleDiameter too small: " ++ show circleDiameter') else circleDiameter'

-- | Given a Drawing, produce a Diagram complete with rotated/flipped icons and
-- lines connecting ports and icons. IO is needed for the GraphViz layout.
renderDrawing ::
  SpecialBackend b Double =>
  Drawing -> IO (SpecialQDiagram b Double)
renderDrawing = renderIconGraph . drawingToIconGraph

renderIngSyntaxGraph ::
  SpecialBackend b Double =>
  Gr (Name, SyntaxNode) Edge -> IO (SpecialQDiagram b Double)
renderIngSyntaxGraph = renderIconGraph . ING.nmap (Control.Arrow.second nodeToIcon)

renderIconGraph :: SpecialBackend b Double => Gr (Name, Icon) Edge -> IO (SpecialQDiagram b Double)
renderIconGraph iconGraph = diagramAction where
  edges = ING.edgeLabel <$> ING.labEdges iconGraph
  diagramAction = doGraphLayout iconGraph edges
