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

-- | A drawing is a map from names to Icons, a list of edges, and a map of names to subDrawings
data Drawing b = Drawing [(Name, Icon)] b [(Name, Drawing b)]

makeNamedMap subDiagramMap =
  map (\(label, dia) -> (label, iconToDiagram dia subDiagramMap # nameDiagram label))

mapFst f = map (\(x, y) -> (f x, y))
toNames = mapFst toName

rawDiagramMap = toNames $
  [("A", Apply0Icon),
  ("B", Apply0Icon),
  ("res", ResultIcon),
  ("foo", TextBoxIcon "foo"),
  ("bar", TextBoxIcon "bar")
  ]

portToPort a b c d = (toName a, Just $ PortName b, toName c, Just $ PortName d)
iconToPort a   c d = (toName a, Nothing, toName c, Just $ PortName d)
iconToIcon a   c   = (toName a, Nothing, toName c, Nothing)

edges =
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

drawing0 = Drawing rawDiagramMap edges []

superIcons = toNames $ [
  ("lam0", LambdaRegionIcon 3 (toName "d0")),
  ("y", TextBoxIcon "y"),
  ("z", TextBoxIcon "z"),
  ("q", TextBoxIcon "q")
  ]

--superDrawing :: (IsName c) => Drawing Name c
--superDrawing = Drawing [((toName "lam0"), LambdaRegionIcon 3 (toName"d0"))] superEdges [((toName "d0"), drawing0)]
superDrawing = Drawing superIcons superEdges [(toName "d0", drawing0)]

edgesToGraph labels edges = mkGraph labels simpleEdges
  where
    simpleEdges = map (\(a, _, c, _) -> (a, c, ())) edges

uncurry4 f (a, b, c, d) = f a b c d

makeConnections edges = applyAll connections
  where
    connections = map (uncurry4 connectMaybePorts) edges

placeNodes scaleFactor layoutResult labelDiagramMap = mconcat placedNodes
  where
    (positionMap, _) = getGraph layoutResult
    placedNodes = map mapper labelDiagramMap
    mapper (label, diagram) = placedNode
      where
        --maybeDiagram = lookup label labelDiagramMap
        placedNode = place
          diagram
          --(fromMaybe (error ("placeNodes: label not in map: " ++ (show (map fst labelDiagramMap)))) maybeDiagram)
          (scaleFactor * positionMap ! label)

-- This is left commented out for a future test of the manual connect functions.
-- connectNodes g =
--   g # connectIconToPort "res" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
--   # connectIconToPort "bar" "B" (PortName 3) # connectPorts "A" (PortName 0) "B" (PortName 2)
--   # connectIconToPort "bar" "A" (PortName 3)


doGraphLayout scaleFactor graph labelDiagramMap connectNodes = do
  layoutResult <- layoutGraph Neato graph
  return $ placeNodes scaleFactor layoutResult labelDiagramMap # connectNodes
  --where
    --diagramScaleConstant = 0.04
    --diagramScaleConstant = 0.1

--main1 = mainWith (ex11 # bgFrame 0.1 black)

--renderDrawing :: Drawing a -> IO (Diagram B)
renderDrawing (Drawing nameIconMap edges subDrawings) scaleFactor = do
  subDiagramMap <- mapM subDrawingMapper subDrawings
  let diagramMap = makeNamedMap subDiagramMap nameIconMap
  doGraphLayout scaleFactor (edgesToGraph iconNames edges) diagramMap $ makeConnections edges
  where
    iconNames = map fst nameIconMap
    subDrawingMapper (label, subDrawing) = do
      subDiagram <- renderDrawing subDrawing (0.4 * scaleFactor)
      return (label, subDiagram)

-- main0 = do
--   placedNodes <- doGraphLayout graph labelDiagramMap connectNodes
--   mainWith (placedNodes # bgFrame 0.1 black)

main1 = do
  placedNodes <- renderDrawing superDrawing 0.1
  mainWith (placedNodes # bgFrame 0.1 black)

main :: IO ()
main = main1
