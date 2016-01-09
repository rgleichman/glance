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
import Rendering

-- todo: Clean up. Put renderDrawing code in a new file.

-- todo: Give graphviz info about the size of the nodes such that a variable scaleFactor
-- todo: Test with more lambdas, (eg. two per layer, 3 or more layers)
-- for subDiagrams is not necessary.

applyDia = apply0Dia
-- --apply0A = "A" .>> applyDia
-- apply0A = applyDia # nameDiagram "A"
-- apply0B = applyDia # nameDiagram "B"
-- result = resultIcon # named "res"
-- fooBox = textBox "foo" # named "foo"
-- barBox = textBox "bar" # named "bar"

-- ex1 = drawIconAndPorts apply0Icon
-- ex2 = drawIconsAndPortNumbers apply0Icon
--ex3 = atPoints (map p2 [(0,0), (3,0)]) [apply0A, apply0B]

-- fromAtoB = ex3 # connectPorts "A" (PortName 0) "B" (PortName 2)
--ex4 = apply0A ||| textBox "hello world" === textBox "1" === textBox "gpq" === textBox ['A'..'Z']

-- ex5 = resultIcon # named "res"||| hrule 1 ||| fromAtoB ||| hrule 1 ||| textBox "foo" # named "foo" === vrule 1 === textBox "bar" # named "bar"
-- ex6 = ex5 # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
--   # connectIconToPort "bar" "B" (PortName 3) # centerXY
--
-- ex7 = ex6 # center # showOrigin # showEnvelope
-- ex8 = enclosure ex6
-- ex9 = lambdaRegion 3 ex6  "lam0"
-- ex10 = ex9 # connectPorts ("lam0" .> "A") (PortName 1) "lam0" (PortName 0)
--   # connectPorts ("lam0" .> "B") (PortName 1) "lam0" (PortName 2)
-- ex11 = connectIcons "lam0" "y" $ ex10 === vrule 2 === textBox "y" # named "y"

d0Icons = toNames
  [("A", Apply0Icon),
  ("B", Apply0Icon),
  ("res", ResultIcon),
  ("foo", TextBoxIcon "foo"),
  ("bar", TextBoxIcon "bar")
  ]

d0Edges =
  [
  portToPort "A" 0 "B" 2,
  iconToPort "foo" "B" 0,
  iconToPort "res" "A" 2,
  iconToPort "foo" "B" 0,
  iconToPort "bar" "B" 3,
  iconToPort "bar" "A" 3
  ]

superEdges =
  [
  portToPort ("lam0" .> "A") 1 "lam0" 0,
  iconToIcon "y" "lam0",
  iconToIcon "z" "lam0",
  iconToIcon "q" "lam0"
  ]

drawing0 = Drawing d0Icons d0Edges []
d0Name = toName "d0"

superIcons = toNames [
  ("lam0", LambdaRegionIcon 3 d0Name),
  ("y", TextBoxIcon "y"),
  ("z", TextBoxIcon "z"),
  ("q", TextBoxIcon "q")
  ]

--superDrawing = Drawing [((toName "lam0"), LambdaRegionIcon 3 (toName"d0"))] superEdges [((toName "d0"), drawing0)]
superDrawing = Drawing superIcons superEdges [(d0Name, drawing0)]

-- This is left commented out for a future test of the manual connect functions.
-- connectNodes g =
--   g # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
--   # connectIconToPort "bar" "B" (PortName 3) # connectPorts "A" (PortName 0) "B" (PortName 2)
--   # connectIconToPort "bar" "A" (PortName 3)

--main1 = mainWith (ex11 # bgFrame 0.1 black)

main1 = do
  placedNodes <- renderDrawing superDrawing 0.1
  mainWith (placedNodes # bgFrame 0.1 black)

main :: IO ()
main = main1
