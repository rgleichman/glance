{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Icons
    (
    Icon(..),
    apply0Dia,
    iconToDiagram,
    --drawIconAndPorts,
    --drawIconsAndPortNumbers,
    nameDiagram,
    connectMaybePorts,
    textBox,
    enclosure,
    lambdaRegion,
    resultIcon,
    guardIcon,
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG(B)
import Data.Maybe (fromMaybe)

-- TYPES --
-- | A datatype that represents an icon.
-- The BranchIcon is used as a branching point for a line.
-- The TextBoxIcon's data is the text that appears in the text box.
-- The LambdaRegionIcon's data is the number of lambda ports, and the name of it's
-- subdrawing.
data Icon = Apply0Icon | ResultIcon | BranchIcon | TextBoxIcon String | GuardIcon Int
  | LambdaRegionIcon Int Name

-- FUNCTIONS --

iconToDiagram Apply0Icon _ = apply0Dia
iconToDiagram ResultIcon _ = resultIcon
iconToDiagram BranchIcon _ = branchIcon
iconToDiagram (TextBoxIcon s) _ = textBox s
iconToDiagram (GuardIcon n) _ = guardIcon n
iconToDiagram (LambdaRegionIcon n diagramName) nameToSubdiagramMap =
  lambdaRegion n dia
  where
    dia = fromMaybe (error "iconToDiagram: subdiagram not found") $ lookup diagramName nameToSubdiagramMap

-- | Names the diagram and puts all sub-names in the namespace of the top level name.
nameDiagram name dia = name .>> (dia # named name)

arrowOptions = with & arrowHead .~ noHead & shaftStyle %~ lwG defaultLineWidth . lc white

connectMaybePorts icon0 (Just port0) icon1 (Just port1) =
  connect'
  arrowOptions
  (icon0 .> port0)
  (icon1 .> port1)
connectMaybePorts icon0 Nothing icon1 (Just port1) =
  connectOutside' arrowOptions icon0 (icon1 .> port1)
connectMaybePorts icon0 (Just port0) icon1 Nothing =
  connectOutside' arrowOptions (icon0 .> port0) icon1
connectMaybePorts icon0 Nothing icon1 Nothing =
  connectOutside' arrowOptions icon0 icon1

-- | Make an port with an integer name. Always use <> to add a ports (not === or |||)
--- since mempty has no size and will not be placed where you want it.
makePort :: Int -> Diagram B
makePort x = mempty # named x
--makePort x = circle 0.2 # fc green # named x
--makePort x = textBox (show x) # fc green # named x


makePortDiagrams points =
  atPoints points (map makePort [0,1..])

-- CONSTANTS --
defaultLineWidth = 0.15

-- APPLY0 ICON --
circleRadius = 0.5
apply0LineWidth = defaultLineWidth

--resultCircle :: Diagram B
resultCircle = circle circleRadius # fc red # lw none

--apply0Triangle :: Diagram B
apply0Triangle = eqTriangle (2 * circleRadius) # rotateBy (-1/12) # fc red # lw none

--apply0Line :: Diagram B
apply0Line = rect apply0LineWidth (2 * circleRadius) # fc white # lw none

--apply0Dia :: Diagram B
apply0Dia = (resultCircle ||| apply0Line ||| apply0Triangle) <> makePortDiagrams apply0PortLocations

apply0PortLocations = map p2 [
  (circleRadius + apply0LineWidth + triangleWidth, 0),
  (lineCenter,circleRadius),
  (-circleRadius,0),
  (lineCenter,-circleRadius)]
  where
    triangleWidth = circleRadius * sqrt 3
    lineCenter = circleRadius + (apply0LineWidth / 2.0)

-- TEXT ICON --
textBoxFontSize = 1
monoLetterWidthToHeightFraction = 0.6
textBoxHeightFactor = 1.1

--textBox :: String -> Diagram B
textBox = coloredTextBox white $ opaque white

-- Since the normal SVG text has no size, some hackery is needed to determine
-- the size of the text's bounding box.
coloredTextBox textColor boxColor t =
  text t # fc textColor # font "freemono" # bold # fontSize (local textBoxFontSize)
  <> rect rectangleWidth (textBoxFontSize * textBoxHeightFactor) # lcA boxColor
  where
    rectangleWidth = textBoxFontSize * monoLetterWidthToHeightFraction
      * fromIntegral (length t)
      + (textBoxFontSize * 0.2)

-- ENCLOSING REGION --
enclosure dia = dia <> boundingRect (dia # frame 0.5) # lc white # lwG defaultLineWidth

-- LAMBDA ICON --
-- Don't use === here to put the port under the text box since mempty will stay
-- at the origin of the text box.
lambdaIcon x = coloredTextBox lime transparent "λ" # alignB <> makePort x

-- LAMBDA REGION --

-- | lambdaRegion takes as an argument the numbers of parameters to the lambda,
-- and draws the diagram inside a region with the lambda icons on top.
lambdaRegion n dia =
  centerXY $ lambdaIcons # centerX === (enclosure dia # centerX)
  where lambdaIcons = hsep 0.4 (take n (map lambdaIcon [0,1..]))

-- RESULT ICON --
resultIcon = unitSquare # lw none # fc lime

-- BRANCH ICON --
branchIcon :: Diagram B
branchIcon = circle 0.3 # fc white # lc white

-- GUARD ICON --
guardTriangle :: Int -> Diagram B
guardTriangle x = triangleAndPort # alignL
  where
    triangleAndPort = polygon (with & polyType .~ PolySides [90 @@ deg, 45 @@ deg] [1, 1])
      # rotateBy (1/8)# lc white # lwG defaultLineWidth # alignT # alignR <> (makePort x # showOrigin)

guardLBracket :: Int -> Diagram B
guardLBracket x = ell # alignT # alignL <> makePort x
  where
    -- todo: use a path or trail here so that the corner is rounded correctly
    ell = (hrule 1 # lc orange # lwG defaultLineWidth # alignR) <> (vrule 1 # lc orange # lwG defaultLineWidth # alignT)

-- | The ports of the guard icon are as follows:
-- Port 0: The top port for the result
-- Ports 1,3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
guardIcon :: Int -> Diagram B
guardIcon n = centerXY $ vcat (take n trianglesAndBrackets # alignT) <> makePort 0
  where
    --guardTriangles = vsep 0.4 (take n (map guardTriangle [0,1..]))
    trianglesWithPorts = map guardTriangle [2,4..]
    lBrackets = map guardLBracket [1,3..]
    trianglesAndBrackets =
      zipWith zipper trianglesWithPorts lBrackets
    zipper tri lBrack = verticalLine === ((lBrack ||| hrule 0.4) # alignR <> (tri # alignL))
      where
        verticalLine = vrule 0.4 # lc white # lwG defaultLineWidth
