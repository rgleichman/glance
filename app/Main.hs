{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.GraphViz

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as GVA
import Data.GraphViz.Commands
import Data.Map((!))
import Data.Maybe (fromMaybe)

import Lib
import Icons

-- todo: refactor and clean up

ex1 = drawIconAndPorts apply0Icon
ex2 = drawIconsAndPortNumbers apply0Icon

applyDia = iconDia apply0Icon
--apply0A = "A" .>> applyDia
apply0A = applyDia # nameDiagram "A"
apply0B = applyDia # nameDiagram "B"
result = resultIcon # named "res"
fooBox = textBox "foo" # named "foo"
barBox = textBox "bar" # named "bar"

graph = mkGraph ["A", "B", "res", "foo", "bar"]
  [("A", "B", ()),
  ("res", "A", ()),
  ("bar", "B", ()),
  ("foo", "B", ())
  ]

labelToDiagram =
  [("A", apply0A),
  ("B", apply0B),
  ("res", result),
  ("foo", fooBox),
  ("bar", barBox)
  ]

ex3 = atPoints (map p2 [(0,0), (3,0)]) [apply0A, apply0B]

fromAtoB = ex3 # connectPorts "A" (PortName 0) "B" (PortName 2)
ex4 = apply0A ||| textBox "hello world" === textBox "1" === textBox "gpq" === textBox ['A'..'Z']

ex5 = resultIcon # named "res"||| hrule 1 ||| fromAtoB ||| hrule 1 ||| textBox "foo" # named "foo" === vrule 1 === textBox "bar" # named "bar"
ex6 = ex5 # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
  # connectIconToPort "bar" "B" (PortName 3) # centerXY

ex7 = ex6 # center # showOrigin # showEnvelope
ex8 = enclosure ex6
ex9 = lambdaRegion 3 ex6 # nameDiagram "lam0"
ex10 = ex9 # connectPorts ("lam0" .> "A") (PortName 1) "lam0" (PortName 0)
  # connectPorts ("lam0" .> "B") (PortName 1) "lam0" (PortName 2)
ex11 = connectIcons "lam0" "y" $ ex10 === vrule 2 === textBox "y" # named "y"


placeNodes layoutResult = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    placedNodes = map (\label -> place (fromMaybe mempty $ lookup label labelToDiagram) (0.04 * positionMap ! label)) $ map fst labelToDiagram

connectNodes g =
  g # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
  # connectIconToPort "bar" "B" (PortName 3) # connectPorts "A" (PortName 0) "B" (PortName 2)

doGraphLayout :: IO (Diagram B)
doGraphLayout = do
  layoutResult <- layoutGraph Neato graph
  return $ placeNodes layoutResult # connectNodes

main1 = mainWith (ex11 # bgFrame 0.1 black)

main0 = do
  placedNodes <- doGraphLayout
  mainWith (placedNodes # bgFrame 0.1 black)

main :: IO ()
main = main0
