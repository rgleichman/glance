{-# LANGUAGE FlexibleContexts #-}
module VisualGraphAlgorithmTests (
  visualCollapseTests
  ) where

import Diagrams.Prelude hiding ((#), (&))

import qualified Data.GraphViz as GV
import qualified Diagrams.TwoD.GraphViz as DiaGV
import qualified Data.GraphViz.Attributes.Complete as GVA

import qualified Data.Graph.Inductive.PatriciaTree as FGR

import Types(SpecialQDiagram, SpecialBackend, SyntaxNode(..), NameAndPort(..), SgNamedNode, Edge(..))
import Translate(translateStringToSyntaxGraph)
import TranslateCore(syntaxGraphToFglGraph)
import GraphAlgorithms(collapseNodes)
import Rendering(customLayoutParams)
import Icons(coloredTextBox)

prettyPrintSyntaxNode :: SyntaxNode -> String
prettyPrintSyntaxNode (NestedApplyNode _ _ namedNodesAndEdges) = concatMap printNameAndEdge namedNodesAndEdges
  where
    printNameAndEdge (namedNode, edge) = "(" ++ prettyPrintNamedNode namedNode ++ "," ++ printEdge edge ++ ")"
    prettyPrintNamedNode = show. fst --  "(" ++ show name ++ "," ++ prettyPrintSyntaxNode syntaxNode ++ ")"
    printEdge (Edge _ _ (NameAndPort n1 _, NameAndPort n2 _)) = show (n1, n2)
prettyPrintSyntaxNode x = show x

renderFglGraph :: SpecialBackend b Double => FGR.Gr SgNamedNode Edge -> IO (SpecialQDiagram b Double)
renderFglGraph fglGraph = do
  layedOutGraph <- DiaGV.layoutGraph' layoutParams GVA.Neato fglGraph
  pure $ DiaGV.drawGraph
    nodeFunc
    --(\_ _ _ _ _ p -> lc white $ stroke p)
    (\_ point1 _ point2 _ _ -> lcA (withOpacity white 0.5) $ arrowBetween (scaleFactor *^ point1) (scaleFactor *^ point2))
    layedOutGraph
  where
    scaleFactor = 0.12
    nodeFunc (name, syntaxNode) point =
      place (coloredTextBox white (opaque white) (show name ++ prettyPrintSyntaxNode syntaxNode) {- :: Diagram B -})
      (scaleFactor *^ point)
    layoutParams :: GV.GraphvizParams Int v e () v
    layoutParams = customLayoutParams{
      GV.fmtNode = nodeAttribute
    }
    nodeAttribute :: (Int, l) -> [GV.Attribute]
    nodeAttribute _ =
      -- GVA.Width and GVA.Height have a minimum of 0.01
      --[GVA.Width diaWidth, GVA.Height diaHeight]
      [GVA.Width 0.01, GVA.Height 0.01]

collapseTestStrings :: [String]
collapseTestStrings = [
  "y = x",
  "y = 1.0",
  "y = f x",
  "y = f x1 x2",
  "y = f (g x)",
  "y = g (\\x -> x)",
  "y = f $ g (\\x -> x)",
  "y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2",
  "Foo x = 1",
  "Foo (Bar x) = 1",
  "Foo 1 x = 2",
  "Foo (Bar x) = f x",
  "y x = case x of {Just w -> (let (z,_) = w in z)}"
  ]

makeCollapseTest :: SpecialBackend b Double => String -> IO (SpecialQDiagram b Double)
makeCollapseTest str = do
  before <- renderFglGraph fglGraph
  afterCollapse <- renderFglGraph collapsedGraph
  pure $ vsep 1 [
    expressionText,
    beforeText,
    before,
    afterText,
    afterCollapse]
  where
    fglGraph = syntaxGraphToFglGraph $ translateStringToSyntaxGraph str
    collapsedGraph = collapseNodes fglGraph
    customTextBox = coloredTextBox white (opaque lime)
    expressionText = alignL $ coloredTextBox white (opaque yellow) str -- :: Diagram B
    beforeText = alignL $ customTextBox "Before:" -- :: Diagram B
    afterText = alignL $ customTextBox "After:" -- :: Diagram B

-- TODO Make this work for many input strings
visualCollapseTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
visualCollapseTests = do
  drawings <- traverse makeCollapseTest collapseTestStrings
  pure $ vsep 1 drawings
