module UnitTests(
  allUnitTests
  ) where

import Test.HUnit

import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR

import Translate(stringToSyntaxGraph)
import TranslateCore(syntaxGraphToFglGraph)
import Types(SgNamedNode, Edge(..), SyntaxNode(..),
             IngSyntaxGraph, NodeName(..), LikeApplyFlavor(..))
import qualified GraphAlgorithms

-- Unit Test Helpers --

assertAllEqual :: (Eq a, Show a) => [a] -> Test
assertAllEqual items = case items of
  [] -> TestCase $ assertFailure "assertAllEqual: argument is empty list"
  (first : rest) -> TestList $ fmap (first ~=?) rest

assertEqualSyntaxGraphs :: [String] -> Test
assertEqualSyntaxGraphs ls = assertAllEqual $ fmap stringToSyntaxGraph ls

-- END Unit Test Helpers --

-- 0:(toName "app02",ApplyNode 1)->[]
-- 1:(toName "f0",LiteralNode "f")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "f0") Nothing,NameAndPort (toName "app02") (Just 0))},0)]
-- 2:(toName "x1",LiteralNode "x")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "x1") Nothing,NameAndPort (toName "app02") (Just 2))},0)]
-- 3:(toName "y3",NameNode "y")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "y3") Nothing,NameAndPort (toName "app02") (Just 1))},0)]
singleApplyGraph :: FGR.Gr SgNamedNode Edge
singleApplyGraph = syntaxGraphToFglGraph $ stringToSyntaxGraph "y = f x"

makeTreeRootTest :: (String, [Maybe SgNamedNode], String) -> Test
makeTreeRootTest (testName, expected, haskellString) = TestCase $ assertEqual testName expected actual where
  actual = fmap (ING.lab graph) treeRoots
  graph = syntaxGraphToFglGraph $ stringToSyntaxGraph haskellString
  treeRoots = GraphAlgorithms.findTreeRoots graph

treeRootTests :: Test
treeRootTests = TestList $ fmap makeTreeRootTest treeRootTestList where
  treeRootTestList = [
    ("single apply", [Just (NodeName 2, LikeApplyNode ApplyNodeFlavor 1)], "y = f x"),
    -- TODO Fix test below
    ("double apply", [Just (NodeName 3, LikeApplyNode ComposeNodeFlavor 2)], "y = f (g x)"),
    ("recursive apply", [Just (NodeName 3,LikeApplyNode ComposeNodeFlavor 2)], "y = f (g y)")
    ]

makeChildCanBeEmbeddedTest ::
  ING.Graph gr =>
  (String, IngSyntaxGraph gr, ING.Node, Bool) -> Test
makeChildCanBeEmbeddedTest (testName, graph, node, expected) =TestCase $ assertEqual testName expected canBeEmbedded where
  canBeEmbedded = GraphAlgorithms.nodeWillBeEmbedded graph node

-- TODO Add more cases for childCanBeEmbeddedTests
-- TODO Fix these tests
childCanBeEmbeddedTests :: Test
childCanBeEmbeddedTests = TestList $ fmap makeChildCanBeEmbeddedTest childCanBeEmbeddedList where
  childCanBeEmbeddedList = [
    ("single apply, ap", singleApplyGraph, 0, False),
    ("single apply, f", singleApplyGraph, 1, True),
    ("single apply, x", singleApplyGraph, 2, True),
    ("single apply, y", singleApplyGraph, 3, False)
    ]

collapseUnitTests :: Test
collapseUnitTests = TestList[
  TestLabel "findTreeRoots" treeRootTests
  --TestLabel "childCanBeEmbedded" childCanBeEmbeddedTests
  ]

-- Translate unit tests

-- Yes, the commas get their own line
translateUnitTests :: Test
translateUnitTests = TestList [
  TestLabel "fmapTest" $ assertEqualSyntaxGraphs [
      "y = fmap f x",
      "y = f <$> x"
      ]
  ,
  TestLabel "dollarTests1" $ assertEqualSyntaxGraphs [
      "y = f x",
      "y = f $ x"
      ]
  ,
  TestLabel "dollarTests2" $ assertEqualSyntaxGraphs [
      "y = f (g x)",
      "y = f $ (g x)",
      "y = f $ g  $ x",
      "y = f (g  $ x)"
      ]
  ,
  TestLabel "dollarTests3" $ assertEqualSyntaxGraphs [
      "y = f 1 (g 2)",
      "y = f 1 $ g 2"
      ]
  ,
  TestLabel "composeApplyTests1" $ assertEqualSyntaxGraphs [
      "y = f (g x)",
      "y = (f . g) x",
      "y = f . g $ x"
      ]
  ,
  TestLabel "composeApplyTests2" $ assertEqualSyntaxGraphs [
      "y = f3 (f2 (f1 x))",
      "y = f3 . f2 . f1 $ x",
      "y = (f3 . f2 . f1) x"
      ]
  ,
  TestLabel "infixTests1" $ assertEqualSyntaxGraphs [
      "y = (+) 1 2",
      "y = ((+) 1) 2",
      "y = 1 + 2",
      "y = (1 +) 2"
      ]
  ,
  TestLabel "infixTests2" $ assertEqualSyntaxGraphs [
      "y = f (1 +) 2",
      "y = f ((+) 1) 2"
      ]
  ]

allUnitTests :: Test
allUnitTests = TestList[
  TestLabel "collapseUnitTests" collapseUnitTests,
  TestLabel "translateTests" translateUnitTests
  ]
