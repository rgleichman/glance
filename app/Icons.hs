{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Icons
    (
    Icon(..),
    apply0Dia,
    iconToDiagram,
    --drawIconAndPorts,
    --drawIconsAndPortNumbers,
    nameDiagram,
    textBox,
    enclosure,
    lambdaRegion,
    resultIcon,
    guardIcon,
    apply0NDia,
    defaultLineWidth
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG(B)
import Data.Maybe (fromMaybe)

import Types(Icon(..), Edge(..))

-- FUNCTIONS --

iconToDiagram Apply0Icon _ = apply0Dia
iconToDiagram (Apply0NIcon n) _ = apply0NDia n
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
apply0Dia = (resultCircle ||| apply0Line ||| apply0Triangle) <> makePortDiagrams apply0PortLocations # centerXY

apply0PortLocations = map p2 [
  (circleRadius + apply0LineWidth + triangleWidth, 0),
  (lineCenter,circleRadius),
  (-circleRadius,0),
  (lineCenter,-circleRadius)]
  where
    triangleWidth = circleRadius * sqrt 3
    lineCenter = circleRadius + (apply0LineWidth / 2.0)

-- apply0N Icon--

apply0NDia :: Int -> Diagram B
apply0NDia n = finalDia # centerXY where
  seperation = 0.6
  trianglePortsCircle = hcat [
    reflectX apply0Triangle,
    hcat $ take n $ map (\x -> makePort x <> strutX seperation) [2,3..],
    makePort 1 <> alignR (circle circleRadius # fc red # lwG defaultLineWidth # lc red)
    ]
  allPorts = makePort 0 <> alignL trianglePortsCircle
  topAndBottomLineWidth = width allPorts - circleRadius
  topAndBottomLine = hrule topAndBottomLineWidth # lc red # lwG defaultLineWidth # alignL
  finalDia = topAndBottomLine === allPorts === topAndBottomLine

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
guardSize = 0.7
guardTriangle :: Int -> Diagram B
guardTriangle x = ((triangleAndPort ||| (hrule (guardSize * 0.8) # lc white # lwG defaultLineWidth)) # alignR) <> (makePort x) # alignL
  where
    triangleAndPort = polygon (with & polyType .~ PolySides [90 @@ deg, 45 @@ deg] [guardSize, guardSize])
      # rotateBy (1/8)# lc white # lwG defaultLineWidth # alignT # alignR

guardLBracket :: Int -> Diagram B
guardLBracket x = ell # alignT # alignL <> makePort x
  where
    -- todo: use a path or trail here so that the corner is rounded correctly
    ell = (hrule guardSize # lc orange # lwG defaultLineWidth # alignR) <> (vrule guardSize # lc orange # lwG defaultLineWidth # alignT)

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
    zipper tri lBrack = verticalLine === ((lBrack ||| strut (guardSize * 0.4)) # alignR <> (tri # alignL))
      where
        verticalLine = vrule 0.4 # lc white # lwG defaultLineWidth
