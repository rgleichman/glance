{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Icons
    (
    apply0Icon,
    Icon(..),
    drawIconAndPorts,
    drawIconsAndPortNumbers
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- | An icon is a Diagram and a list of points for the ports to the diagram.
-- The first portection is the right most (ie. 0 degrees), and other ports
-- proceed counterclockwise.
data Icon a = Icon {iconDia :: Diagram a, ports :: [P2 Double]}

-- | Draw the icon with circles where the ports are
drawIconAndPorts :: Icon B -> Diagram B
drawIconAndPorts (Icon dia port) =
  vertCircles <> dia
  where
    vertCircles = atPoints port $ repeat $ circle 0.05 # lw none # fc blue

drawIconsAndPortNumbers :: Icon B -> Diagram B
drawIconsAndPortNumbers (Icon dia port) =
  portNumbers <> dia
  where
    portNumbers = atPoints port $ map makeNumDia [0,1..]
    makeNumDia num = text (show num) # fontSize (local 0.1) # fc blue <> square 0.1 # fc white

circleRadius = 0.5
apply0LineWidth = 0.25

resultCircle :: Diagram B
resultCircle = circle circleRadius # fc red # lw none

apply0Triangle :: Diagram B
apply0Triangle = eqTriangle (2 * circleRadius) # rotateBy (-1/12) # fc red # lw none

apply0Line :: Diagram B
apply0Line = rect apply0LineWidth (2 * circleRadius) # fc white # lw none

apply0Dia :: Diagram B
apply0Dia = resultCircle ||| apply0Line ||| apply0Triangle

apply0Icon :: Icon B
apply0Icon = Icon apply0Dia verts

verts = map p2 [
  (circleRadius + apply0LineWidth + triangleWidth, 0),
  (lineCenter,circleRadius),
  (-circleRadius,0),
  (lineCenter,-circleRadius)]
  where
    triangleWidth = circleRadius * sqrt 3
    lineCenter = circleRadius + (apply0LineWidth / 2.0)
