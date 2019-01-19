{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, PartialTypeSignatures, ScopedTypeVariables #-}

module Rendering (
  renderDrawing,
  customLayoutParams,
  renderIngSyntaxGraph
) where

import Diagrams.Prelude(toName, shaftStyle, global, arrowShaft, noTail
                       , arrowTail, noHead, arrowHead, scale, r2, bezier3
                       , fromSegments, Angle, P2, V2, Point, Name, ArrowOpts, N
                       , TrailLike, V, height, width, (*^), reflectX, rotate
                       , centerXY, place
                       , roundedRect, dashingG, lwG, lightgreen, lc, centerPoint
                       , moveTo, turn, (@@), unitX, signedAngleBetween, (.-.)
                       , applyAll, names, angleV, rad, (^.), angleBetween, (.>)
                       , connectOutside', connect', with, (%~), lengths, (^+^)
                       , (.~))
import Diagrams.TwoD.GraphViz(mkGraph, getGraph, layoutGraph')
import qualified Diagrams.Prelude as DIA

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Map as Map

import Data.Function(on)
import qualified Data.Graph.Inductive as ING
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List(find, minimumBy)
import Data.Maybe(fromMaybe)
import Data.Typeable(Typeable)

--import qualified Data.GraphViz.Types
--import Data.GraphViz.Commands
--import qualified Debug.Trace

import Icons(colorScheme, iconToDiagram, defaultLineWidth, ColorStyle(..)
            , getPortAngles, TransformParams(..), circleRadius)
import TranslateCore(nodeToIcon)
import Types(Edge(..), EdgeOption(..), Drawing(..), EdgeEnd(..), NameAndPort(..)
            , SpecialQDiagram, SpecialBackend, SpecialNum, NodeName(..)
            , Port(..), SgNamedNode, NamedIcon(..), Icon(..))

import Util(fromMaybeError, mapNodeInNamedNode, namedIconToTuple)

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

-- TODO Refactor with syntaxGraphToFglGraph in TranslateCore
-- TODO Make this work with nested icons now that names are not qualified.
drawingToIconGraph :: Drawing -> Gr NamedIcon Edge
drawingToIconGraph (Drawing nodes edges) =
  mkGraph nodes labeledEdges where
    labeledEdges = fmap makeLabeledEdge edges
    makeLabeledEdge e@(Edge _ _ (NameAndPort n1 _, NameAndPort n2 _)) =
      (NamedIcon n1 (lookupInNodes n1), NamedIcon n2 (lookupInNodes n2), e)
      where
        lookupInNodes name = fromMaybeError
                             errorString
                             (lookup name (fmap namedIconToTuple nodes))
          where
            errorString =
              "syntaxGraphToFglGraph edge connects to non-existent node. Node NodeName ="
              ++ show name ++ " Edge=" ++ show e


bezierShaft :: (V t ~ V2, TrailLike t) =>
  Angle (N t) -> Angle (N t) -> t
bezierShaft angle1 angle2 = fromSegments [bezier3 c1 c2 x] where
  scaleFactor = 0.5
  x = r2 (1,0)
  c1 = rotate angle1 (scale scaleFactor unitX)
  c2 = rotate angle2 (scale scaleFactor unitX) ^+^ x

getArrowOpts :: (RealFloat n, Typeable n) =>
  (EdgeEnd, EdgeEnd)
  -> [EdgeOption]
  -> (Angle n, Angle n)
  -> NameAndPort
  -> (ArrowOpts n, DIA.Colour Double)
getArrowOpts (t, h)
  _
  (fromAngle, toAngle)
  (NameAndPort (NodeName nodeNum) mPort)
  = (arrowOptions, shaftColor)
  where
    -- shaftColor = if EdgeInPattern `elem` opts
    --              then patternC colorScheme
    --              else hashedColor
    shaftColor = hashedColor

    edgeColors = edgeListC colorScheme
    numEdgeColors = length edgeColors
    hashedColor = edgeColors !! namePortHash
    namePortHash = mod (portNum + (503 * nodeNum)) numEdgeColors
    Port portNum = fromMaybe (Port 0) mPort

    lookupTail EndNone = id

    lookupHead EndNone = id

    arrowOptions =
      arrowHead .~ noHead $
      arrowTail .~ noTail $
      arrowShaft .~ bezierShaft fromAngle toAngle $
      lengths .~ global 0.75 $
      lookupHead h $ lookupTail t with

-- | Given an Edge, return a transformation on Diagrams that will draw a line.
connectMaybePorts :: SpecialBackend b n =>
  (Angle n, Angle n)-> Edge -> SpecialQDiagram b n -> SpecialQDiagram b n
connectMaybePorts portAngles
  (Edge
    opts
    ends
    (fromNamePort@(NameAndPort name0 mPort1), NameAndPort name1 mPort2))
  -- In order to give arrows a "shadow" effect, draw a thicker semi-transparent
  -- line shaft the same color as the background underneath the normal line
  -- shaft.
  = connectFunc normalOpts qPort0 qPort1
    . connectFunc arrOptsShadow qPort0 qPort1
  where
    lineWidth = 2 * defaultLineWidth
    (baseArrOpts, shaftCol) = getArrowOpts ends opts portAngles fromNamePort
    normalOpts = (shaftStyle %~ (lwG lineWidth . lc shaftCol))
                 baseArrOpts
    arrOptsShadow = (shaftStyle
                     %~ (lwG (1.9 * lineWidth)
                         . DIA.lcA
                          $ DIA.withOpacity (backgroundC colorScheme) 0.5))
                    baseArrOpts
    (connectFunc, qPort0, qPort1) = case (mPort1, mPort2) of
      (Just port0, Just port1) -> (connect', name0 .> port0, name1 .> port1)
      (Nothing, Just port1) -> (connectOutside', toName name0, name1 .> port1)
      (Just port0, Nothing) -> (connectOutside', name0 .> port0, toName name1)
      (_, _) -> (connectOutside', toName name0, toName name1)

-- START addEdges --
nameAndPortToName :: NameAndPort -> Name
nameAndPortToName (NameAndPort name mPort) = case mPort of
  Nothing -> toName name
  Just port -> name .> port

findPortAngles :: SpecialNum n => NamedIcon -> NameAndPort -> [Angle n]
findPortAngles (NamedIcon nodeName nodeIcon) (NameAndPort diaName mPort)
  = case mPort of
      Nothing -> []
      Just port -> foundAngles where
        mName = if nodeName == diaName then Nothing else Just diaName
        foundAngles = getPortAngles nodeIcon port mName

-- TODO Clean up the Angle arithmatic
pickClosestAngle :: SpecialNum n =>
  (Bool, Angle n)
  -> Angle n
  -> Angle n
  -> Angle n
  -> [Angle n]
  -> Angle n
pickClosestAngle (nodeFlip, nodeAngle) emptyCase target shaftAngle angles
  = case angles of
      [] -> emptyCase
      _ -> (-) <$>
        fst (minimumBy (compare `on` snd) $ fmap angleDiff adjustedAngles)
        <*>
        shaftAngle
        where
          adjustedAngles = fmap adjustAngle angles
          angleDiff angle = (angle, angleBetween (angleV target) (angleV angle))

          adjustAngle angle = if nodeFlip
                              then signedAngleBetween
                                   (rotate nodeAngle $ reflectX (angleV angle))
                                   unitX
                              else (+) <$> angle <*> nodeAngle


-- TODO Refactor with pickClosestAngle
smallestAngleDiff :: SpecialNum n =>
  (Bool, Angle n) -> Angle n -> [Angle n] -> n
smallestAngleDiff (nodeFlip, nodeAngle) target angles = case angles of
  [] -> 0
  _ -> minimum $ fmap angleDiff adjustedAngles
    where
      adjustedAngles = fmap adjustAngle angles
      angleDiff angle = angleBetween (angleV target) (angleV angle) ^. rad

      adjustAngle angle = if nodeFlip then
        signedAngleBetween (rotate nodeAngle $ reflectX (angleV angle)) unitX
        else
        (+) <$> angle <*> nodeAngle


lookupNodeAngle ::  Show n =>
  [(NamedIcon, (Bool, Angle n))] -> NamedIcon -> (Bool, Angle n)
lookupNodeAngle rotationMap key
  = fromMaybeError
  ("nodeVector: key not in rotaionMap. key = " ++ show key
    ++ "\n\n rotationMap = " ++ show rotationMap)
  $ lookup key rotationMap

makeEdge :: (SpecialBackend b n, ING.Graph gr) =>
  gr NamedIcon Edge
  -> SpecialQDiagram b n
  -> [(NamedIcon, (Bool, Angle n))]
  -> ING.LEdge Edge
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
makeEdge graph dia rotationMap
  (node0, node1, edge@(Edge _ _ (namePort0, namePort1)))
  = connectMaybePorts portAngles edge
  where
    node0label = fromMaybeError
                 ("makeEdge: node0 is not in graph. node0: " ++ show node0)
                 $ ING.lab graph node0
    node1label = fromMaybeError
                 ("makeEdge: node1 is not in graph. node1: " ++ show node1)
                 $ ING.lab graph node1

    node0Angle = lookupNodeAngle rotationMap node0label
    node1Angle = lookupNodeAngle rotationMap node1label

    diaNodeNamePointMap = names dia
    port0Point = getPortPoint $ nameAndPortToName namePort0
    port1Point = getPortPoint $ nameAndPortToName namePort1
    shaftVector = port1Point .-. port0Point
    shaftAngle = signedAngleBetween shaftVector unitX

    icon0PortAngle = pickClosestAngle node0Angle mempty shaftAngle shaftAngle
                     $ findPortAngles node0label namePort0

    shaftAnglePlusOneHalf = (+) <$> shaftAngle <*> (1/2 @@ turn)
    icon1PortAngle = pickClosestAngle
                     node1Angle
                     (1/2 @@ turn)
                     shaftAnglePlusOneHalf
                     shaftAngle
                     (findPortAngles node1label namePort1)

    getPortPoint n = case foundPoints of
      [point] -> point
      _ -> error $ "Multiple points with named: " <> show n
      where
        foundPoints = fromMaybeError
          ( "makeEdge: port not found. Port: " ++ show n ++ ". Valid ports: "
            ++ show diaNodeNamePointMap)
          (lookup n diaNodeNamePointMap)

    portAngles = (icon0PortAngle, icon1PortAngle)

-- | addEdges draws the edges underneath the nodes.
addEdges :: (SpecialBackend b n, ING.Graph gr) =>
  gr NamedIcon Edge
  -> SpecialQDiagram b n
  -> [(NamedIcon, (Bool, Angle n))]
  -> SpecialQDiagram b n
addEdges graph dia rotationMap = applyAll connections dia
  where
    connections = makeEdge graph dia rotationMap <$> ING.labEdges graph

-- BEGIN rotateNodes --

-- TODO May want to use a power other than 2 for the edgeAngleDiffs
scoreAngle :: SpecialNum n =>
  Point V2 n
  -> [(Point V2 n, [Angle n])]
  -> Bool
  -> Angle n
  -> n
scoreAngle iconPosition edges reflected angle
  = sum $ (^(2 :: Int)) <$> fmap edgeAngleDiff edges
  where
    edgeAngleDiff (otherNodePosition, portAngles) = angleDiff
      where
        shaftVector = otherNodePosition .-. iconPosition
        shaftAngle = signedAngleBetween shaftVector unitX
        angleDiff = smallestAngleDiff (reflected, angle) shaftAngle portAngles

bestAngleForIcon :: (SpecialNum n, ING.Graph gr) =>
  Map.Map NamedIcon (Point V2 n)
  -> gr NamedIcon Edge
  -> NamedIcon
  -> Bool
  -> (Angle n, n)
bestAngleForIcon positionMap graph key@(NamedIcon (NodeName nodeId) _) reflected
  = minimumBy (compare `on` snd)
    ( (\angle -> (angle
                 , scoreAngle iconPosition edges reflected angle))
      <$> fmap (@@ turn) possibleAngles)
  where
    possibleAngles = [0,(1/24)..1]
    -- possibleAngles = [0, 1/2] -- (uncomment this line and comment out the line above to disable rotation)
    iconPosition = positionMap Map.! key
    edges = getPositionAndAngles
            <$> ( fmap getSucEdge (ING.lsuc graph nodeId)
                  <> fmap getPreEdge (ING.lpre graph nodeId))

    getPositionAndAngles (node, nameAndPort)
      = (positionMap Map.! nodeLabel, portAngles)
      where
        nodeLabel = fromMaybeError
                    "getPositionAndAngles: node not found"
                    (ING.lab graph node)
        portAngles = findPortAngles key nameAndPort

  -- Edge points from id to otherNode
    getSucEdge (otherNode, edge) = (otherNode, nameAndPort) where
      (nameAndPort, _) = edgeConnection edge

  -- Edge points from otherNode to id
    getPreEdge (otherNode, edge) = (otherNode, nameAndPort) where
      (_, nameAndPort) = edgeConnection edge

findIconRotation :: (SpecialNum n, ING.Graph gr) =>
  Map.Map NamedIcon (Point V2 n)
  -> gr NamedIcon Edge
  -> NamedIcon
  -> (NamedIcon, (Bool, Angle n))
findIconRotation positionMap graph key = (key, (reflected, angle)) where
  -- Smaller scores are better
  (reflectedAngle, reflectedScore) = bestAngleForIcon positionMap graph key True
  (nonReflectedAngle, nonReflectedScore)
    = bestAngleForIcon positionMap graph key False
  reflected = reflectedScore < nonReflectedScore
  angle = if reflected then reflectedAngle else nonReflectedAngle

rotateNodes :: (SpecialNum n, ING.Graph gr) =>
  Map.Map NamedIcon (Point V2 n)
  -> gr NamedIcon Edge
  -> [(NamedIcon, (Bool, Angle n))]
rotateNodes positionMap graph
  = findIconRotation positionMap graph <$> Map.keys positionMap

-- END rotateNodes --

drawLambdaRegions :: SpecialBackend b Double =>
  [(NamedIcon, SpecialQDiagram b Double)]
  -> SpecialQDiagram b Double
drawLambdaRegions placedNodes
  = mconcat $ fmap (drawRegion . niIcon . fst) placedNodes
  where
    drawRegion (FlatLambdaIcon _ enclosedNames)
      = regionRect enclosedDias
      where
        enclosedDias = fmap findDia enclosedNames
        findDia n1
          = fromMaybe mempty
            $ snd <$> find (\(NamedIcon n2 _, _) -> n1 == n2) placedNodes
    drawRegion _ = mempty

    -- TODO Use something better than a rectangle
    regionRect dias
      = moveTo (centerPoint combinedDia)
        $ lc lightgreen (lwG defaultLineWidth contentsRect)
      where
        combinedDia = mconcat dias
        rectPadding = 3 * circleRadius
        contentsRect = dashingG [0.4 * circleRadius, 0.8 * circleRadius] 0
                       $ roundedRect
                       (rectPadding + width combinedDia)
                       (rectPadding + height combinedDia)
                       (3 * circleRadius)

placeNodes :: SpecialBackend b Double =>
  Map.Map NamedIcon (P2 Double)
  -> [(NamedIcon, (Bool, Angle Double))]
  -> [(NamedIcon, SpecialQDiagram b Double)]
placeNodes positionMap = fmap placeNode
  where
    placeNode (key@(NamedIcon name icon), (reflected, angle))
      = (key, place transformedDia diaPosition)
      where
        origDia = centerXY
                  $ iconToDiagram icon (TransformParams name 0 reflected angle)
        transformedDia = centerXY $ rotate angle
                         $ (if reflected then reflectX else id) origDia
        diaPosition = graphvizScaleFactor *^ (positionMap Map.! key)


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
  Gr NamedIcon Edge
  -> IO (SpecialQDiagram b Double)
doGraphLayout graph = do
  layoutResult <- layoutGraph' layoutParams GVA.Neato graph
  --  layoutResult <- layoutGraph' layoutParams GVA.Fdp graph
  let
    positionMap = fst $ getGraph layoutResult
    rotationMap = rotateNodes positionMap graph
    placedNodeList = placeNodes positionMap rotationMap
    placedNodes = mconcat $ fmap snd placedNodeList
    edges = addEdges graph placedNodes rotationMap
    placedRegions = drawLambdaRegions placedNodeList
  pure (placedNodes <> edges <> placedRegions)
  where
    layoutParams :: GV.GraphvizParams Int NamedIcon e () NamedIcon
    --layoutParams :: GV.GraphvizParams Int l el Int l
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
      }
    nodeAttribute :: (Int, NamedIcon) -> [GV.Attribute]
    nodeAttribute (_, NamedIcon _ nodeIcon) =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width circleDiameter, GVA.Height circleDiameter]
      where
        -- This type annotation (:: SpecialQDiagram b n) requires Scoped Typed
        -- Variables, which only works if the function's
        -- type signiture has "forall b e."
        dia :: SpecialQDiagram b Double
        dia = iconToDiagram
              nodeIcon
              (TransformParams (NodeName (-1)) 0 False mempty)

        diaWidth = drawingToGraphvizScaleFactor * width dia
        diaHeight = drawingToGraphvizScaleFactor * height dia
        circleDiameter' = max diaWidth diaHeight
        circleDiameter
          = if circleDiameter' <= 0.01
            then error ("circleDiameter too small: " ++ show circleDiameter')
            else circleDiameter'

-- | Given a Drawing, produce a Diagram complete with rotated/flipped icons and
-- lines connecting ports and icons. IO is needed for the GraphViz layout.
renderDrawing ::
  SpecialBackend b Double =>
  Drawing -> IO (SpecialQDiagram b Double)
renderDrawing = renderIconGraph . drawingToIconGraph

renderIngSyntaxGraph ::
  SpecialBackend b Double =>
  Gr SgNamedNode Edge -> IO (SpecialQDiagram b Double)
renderIngSyntaxGraph
  = renderIconGraph . ING.nmap (mapNodeInNamedNode nodeToIcon)

renderIconGraph :: SpecialBackend b Double
                => Gr NamedIcon Edge -> IO (SpecialQDiagram b Double)
renderIconGraph = doGraphLayout
