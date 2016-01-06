{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Icons
    (
    apply0Icon,
    Icon(..),
    drawIconAndPorts,
    drawIconsAndPortNumbers,
    PortName(..),
    nameDiagram,
    connectPorts,
    connectIconToPort,
    connectIcons,
    textBox,
    enclosure,
    lambdaRegion,
    resultIcon
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- | An icon is a Diagram and a list of points for the ports to the diagram.
-- The first portection is the right most (ie. 0 degrees), and other ports
-- proceed counterclockwise.
data Icon a = Icon {iconDia :: Diagram a, ports :: [P2 Double]}

-- | PortName is a simple wrapper around Int that is used for the diagram names
-- of all the ports.
instance IsName PortName
newtype PortName = PortName Int deriving (Show, Ord, Eq)

defaultLineWidth = 0.15

-- | Names the diagram and puts all sub-names in the namespace of the top level name.
nameDiagram name dia = name .>> (dia # named name)

arrowOptions = with & arrowHead .~ noHead & shaftStyle %~ lwG defaultLineWidth . lc white

connectPorts icon0 port0 icon1 port1 =
  connect'
  arrowOptions
  (icon0 .> port0)
  (icon1 .> port1)

connectIconToPort icon0 icon1 port1 =
  connectOutside' arrowOptions icon0 (icon1 .> port1)

connectIcons =
  connectOutside' arrowOptions

-- | Draw the icon with circles where the ports are
drawIconAndPorts :: Icon B -> Diagram B
drawIconAndPorts (Icon dia ports) =
  vertCircles <> dia
  where
    vertCircles = atPoints ports $ repeat $ circle 0.05 # lw none # fc blue

drawIconsAndPortNumbers :: Icon B -> Diagram B
drawIconsAndPortNumbers (Icon dia ports) =
  portNumbers <> dia
  where
    portNumbers = atPoints ports $ map makeNumDia [0,1..]
    makeNumDia num = text (show num) # fontSize (local 0.1) # fc blue <> square 0.1 # fc white

-- APPLY 0 ICON --
circleRadius = 0.5
apply0LineWidth = defaultLineWidth

resultCircle :: Diagram B
resultCircle = circle circleRadius # fc red # lw none

apply0Triangle :: Diagram B
apply0Triangle = eqTriangle (2 * circleRadius) # rotateBy (-1/12) # fc red # lw none

apply0Line :: Diagram B
apply0Line = rect apply0LineWidth (2 * circleRadius) # fc white # lw none

apply0Dia :: Diagram B
apply0Dia = (resultCircle ||| apply0Line ||| apply0Triangle) <> makePortDiagrams verts

apply0Icon :: Icon B
apply0Icon = Icon apply0Dia verts

makePort x = mempty # named (PortName x)

makePortDiagrams points =
  atPoints points (map makePort [0,1..])

verts = map p2 [
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

textBox :: String -> Diagram B
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
lambdaRegion n dia = hsep 0.4 (take n (map lambdaIcon [0,1..])) # center === enclosure dia

-- RESULT ICON --
resultIcon = unitSquare # lw none # fc lime
