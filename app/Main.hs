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

-- todo: Put

applyDia = iconDia apply0Icon
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

makeNamedMap :: (IsName a) => [(a, Diagram B)] -> [(a, Diagram B)]
makeNamedMap =
  map (\(label, dia) -> (label, dia # nameDiagram label))

labelDiagramMap = makeNamedMap
  [("A", applyDia ),
  ("B", applyDia),
  ("res", resultIcon),
  ("foo", textBox "foo"),
  ("bar", textBox "bar")
  ]

labels = map fst labelDiagramMap

portToPort a b c d = (a, Just $ PortName b, c, Just $ PortName d)
iconToPort a   c d = (a, Nothing, c, Just $ PortName d)
iconToIcon a   c   = (a, Nothing, c, Nothing)

edges =
  [
  portToPort "A" 0 "B" 2,
  iconToPort "foo" "B" 0,
  iconToPort "res" "A" 2,
  iconToPort "foo" "B" 0,
  iconToPort "bar" "B" 3,
  iconToPort "bar" "A" 3
  ]

edgesToGraph labels edges = mkGraph labels simpleEdges
  where
    simpleEdges = map (\(a, _, c, _) -> (a, c, ())) edges

graph = edgesToGraph labels edges

uncurry4 f (a, b, c, d) = f a b c d

makeConnections edges = applyAll connections
  where
    connections = map (uncurry4 connectMaybePorts) edges

placeNodes scaleFactor layoutResult labelDiagramMap = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    placedNodes = map mapper labels
    mapper label = placedNode
      where
        maybeDiagram = lookup label labelDiagramMap
        placedNode = place
          (fromMaybe (error "placeNodes: label not in map") maybeDiagram)
          (scaleFactor * positionMap ! label)

-- This is left commented out for a future test of the manual connect functions.
-- connectNodes g =
--   g # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
--   # connectIconToPort "bar" "B" (PortName 3) # connectPorts "A" (PortName 0) "B" (PortName 2)
--   # connectIconToPort "bar" "A" (PortName 3)

connectNodes = makeConnections edges

doGraphLayout graph labelDiagramMap connectNodes = do
  layoutResult <- layoutGraph Neato graph
  return $ placeNodes 0.04 layoutResult labelDiagramMap # connectNodes

--main1 = mainWith (ex11 # bgFrame 0.1 black)

main0 = do
  placedNodes <- doGraphLayout graph labelDiagramMap connectNodes
  mainWith (placedNodes # bgFrame 0.1 black)

main :: IO ()
main = main0
