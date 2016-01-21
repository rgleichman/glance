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

import Data.Typeable(Typeable)

import Lib
import Icons(apply0Dia, apply0NDia)
import Rendering(toNames, portToPort, iconToPort, iconToIcon, renderDrawing)
import Types(Icon(..), Drawing(..))

-- todo: Find out how to hide unqualified names such that recursive drawings are connected correctly
-- todo: Find out and fix why connectinos to sub-icons need to be qualified twice (eg. "lam0" .> "arg" .> "arg")
-- todo: Rotate based on difference from ideal tangent angle, not line distance.
-- todo: layout and rotate considering external connections.
-- todo: add port to bottom of guard.
-- todo: use constants for icon name strings in Main
-- todo: figure out local vs. global icon positions
-- todo: replace hrule and vrule with strutX and strutY

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

drawing0 = Drawing d0Icons d0Edges []
d0Name = toName "d0"

superEdges =
  [
  portToPort ("lam0" .> "A") 1 "lam0" 0,
  iconToIcon "y" "lam0",
  iconToIcon "z" "lam0",
  iconToIcon "q" "lam0",
  iconToIcon "A" "z",
  iconToPort ("lam0" .> "foo" .> "foo") "lam0" 0
  ]

superIcons = toNames [
  ("lam0", LambdaRegionIcon 3 d0Name),
  ("y", TextBoxIcon "y"),
  ("z", TextBoxIcon "z"),
  ("q", TextBoxIcon "q")
  ]

--superDrawing = Drawing [((toName "lam0"), LambdaRegionIcon 3 (toName"d0"))] superEdges [((toName "d0"), drawing0)]
superDrawing = Drawing superIcons superEdges [(d0Name, drawing0)]

super2Icons = toNames [
  ("lam0", LambdaRegionIcon 1 d0Name),
  --("y", TextBoxIcon "y"),
  ("lam1", LambdaRegionIcon 2 d0Name)
  ]

super2Edges =
  [
  iconToIcon "lam0" "lam1"
  --iconToIcon "y" "lam0"
  ]

super2Drawing = Drawing super2Icons super2Edges [(d0Name, drawing0)]
super2Name = toName "s2"

super3Icons = toNames [
  ("lam0", LambdaRegionIcon 3 d1Name),
  --("y", TextBoxIcon "y"),
  ("lam1", LambdaRegionIcon 4 d1Name)
  ]

super3Edges =
  [
--  iconToIcon "lam0" "lam1",
  iconToIcon "lam0" "A"
  ]
d1Name = toName "d1"
super3Drawing = Drawing super3Icons super2Edges [(d1Name, super2Drawing)]

fact0Icons = toNames
  [
  ("g0", GuardIcon 2),
  ("one", TextBoxIcon "1"),
  ("eq0", TextBoxIcon "== 0"),
  ("-1", TextBoxIcon "-1"),
  ("eq0Ap", Apply0Icon),
  ("-1Ap", Apply0Icon),
  ("*", TextBoxIcon "*"),
  ("recurAp", Apply0Icon),
  ("*Ap", Apply0NIcon 2),
  --("*Ap1", Apply0Icon),
  --("*Ap2", Apply0Icon),
  ("arg", BranchIcon),
  ("res", ResultIcon)
  ]

fact0Edges = [
    iconToPort "eq0" "eq0Ap" 0,
    portToPort "eq0Ap" 2 "g0" 1,
    iconToPort "-1" "-1Ap" 0,
    iconToPort "*" "*Ap" 0,
    iconToPort "one" "g0" 2,
    portToPort "*Ap" 1 "g0" 4,
    --portToPort "*Ap" 3 "recurAp" 0,
    portToPort "recurAp" 2 "*Ap" 3,
    iconToPort "arg" "eq0Ap" 1,
    iconToPort "arg" "-1Ap" 1,
    iconToPort "arg" "*Ap" 2,
    portToPort "-1Ap" 2 "recurAp" 1,
    iconToPort "res" "g0" 0
  ]

fact0Drawing = Drawing fact0Icons fact0Edges []
fact0Name = toName "fac0"

factLam0Icons = toNames [
  ("lam0", LambdaRegionIcon 1 fact0Name),
  ("fac", TextBoxIcon "factorial")
  ]

factLam0Edges = [
  iconToPort ("lam0" .> "arg" .> "arg") "lam0" 0,
  iconToPort "lam0" ("lam0" .> "recurAp") 0,
  --portToPort "lam0" 0 ("lam0" .> "*Ap2") 3,
  iconToIcon "lam0" "fac"
  ]

factLam0Drawing = Drawing factLam0Icons factLam0Edges [(fact0Name, fact0Drawing)]

-- This is left commented out for a future test of the manual connect functions.
-- connectNodes g =
--   g # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
--   # connectIconToPort "bar" "B" (PortName 3) # connectPorts "A" (PortName 0) "B" (PortName 2)
--   # connectIconToPort "bar" "A" (PortName 3)

--main1 = mainWith (ex11 # bgFrame 0.1 black)

main1 :: IO ()
main1 = do
  placedNodes <- renderDrawing factLam0Drawing
  mainWith (placedNodes # bgFrame 0.1 black)

main2 = mainWith (apply0NDia 3 # bgFrame 0.1 black)

main :: IO ()
main = main1
