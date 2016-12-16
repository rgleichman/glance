module UnitTests(
  allUnitTests
  ) where

import Test.HUnit

import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR

import Data.List(foldl', sort, sortOn)

import Translate(translateStringToSyntaxGraph)
import TranslateCore(syntaxGraphToFglGraph, SyntaxGraph(..), Reference)
import Types(SgNamedNode, Edge(..), SyntaxNode(..),
             IngSyntaxGraph, NodeName(..), LikeApplyFlavor(..), NameAndPort(..))
import qualified GraphAlgorithms
import Util(fromMaybeError)

-- Unit Test Helpers --

assertAllEqual :: (Eq a, Show a) => [a] -> Test
assertAllEqual items = case items of
  [] -> TestCase $ assertFailure "assertAllEqual: argument is empty list"
  (first : rest) -> TestList $ fmap (first ~=?) rest

assertEqualSyntaxGraphs :: [String] -> Test
assertEqualSyntaxGraphs ls = assertAllEqual $ fmap (renameGraph . translateStringToSyntaxGraph) ls

-- BEGIN renameGraph --
type NameMap = [(NodeName, NodeName)]

renameNode
  :: NameMap -> Int -> SgNamedNode -> (SgNamedNode, NameMap, Int)
renameNode nameMap counter (nodeName, syntaxNode) = (newNamedNode, nameMap3, newCounter) where
  newNodeName = NodeName counter
  nameMap2 = (nodeName, newNodeName) : nameMap
  (newSyntaxNode, nameMap3, newCounter) = renameSyntaxNode nameMap2 syntaxNode (counter + 1)
  newNamedNode = (newNodeName, newSyntaxNode)

maybeRenameNodeFolder ::
  ([Maybe SgNamedNode], NameMap, Int) -> Maybe SgNamedNode -> ([Maybe SgNamedNode], NameMap, Int)
maybeRenameNodeFolder (renamedNodes, nameMap, counter) mNode = case mNode of
  Nothing -> (Nothing:renamedNodes, nameMap, counter)
  Just node -> (Just newNamedNode:renamedNodes, newNameMap, newCounter) where
    (newNamedNode, newNameMap, newCounter) = renameNode nameMap counter node

renameSyntaxNode :: NameMap -> SyntaxNode -> Int  -> (SyntaxNode, NameMap, Int)
renameSyntaxNode nameMap node counter = case node of
  -- TODO Keep the Nothing subNodes
  NestedPatternApplyNode s subNodes -> (NestedPatternApplyNode s (reverse renamedSubNodes), newNameMap, counter2)
    where
      (renamedSubNodes, newNameMap, counter2) = foldl' maybeRenameNodeFolder ([], nameMap, counter) subNodes
  _ -> (node, nameMap, counter)

renameNodeFolder :: ([SgNamedNode], NameMap, Int) -> SgNamedNode -> ([SgNamedNode], NameMap, Int)
renameNodeFolder state@(renamedNodes, nameMap, counter) node@(nodeName, _) = case lookup nodeName nameMap of
  Nothing -> (newNamedNode:renamedNodes, newNameMap, newCounter) where
    (newNamedNode, newNameMap, newCounter) = renameNode nameMap counter node
  Just _ -> error $ "renameNode: node already in name map. State = " ++ show state ++ " Node = " ++ show node

renameNamePort :: NameMap -> NameAndPort -> NameAndPort
renameNamePort nameMap nameAndPort@(NameAndPort name port) = NameAndPort newName port where
  newName = fromMaybeError errorStr $ lookup name nameMap
  errorStr = "renameNamePort: name not found. name = " ++ show name ++ "\nNameAndPort = " ++ show nameAndPort ++ "\nNameMap = " ++ show nameMap

renameEdge :: NameMap -> Edge -> Edge
renameEdge nameMap (Edge options ends (np1, np2)) =
  Edge options ends (renameNamePort nameMap np1, renameNamePort nameMap np2)

renameSource :: NameMap -> (String, Reference) -> (String, Reference)
renameSource nameMap (str, ref) = (str, newRef) where
  newRef = case ref of
    Left _ -> ref
    Right namePort@(NameAndPort _ _) -> Right $ renameNamePort nameMap namePort

renameEmbed :: NameMap -> (NodeName, NodeName) -> (NodeName, NodeName)
renameEmbed nameMap (leftName, rightName) = (newLeftName, newRightName) where
  newLeftName = fromMaybeError "renameEmbed: leftName not found" (lookup leftName nameMap)
  newRightName = fromMaybeError "renameEmbed: RightName not found" (lookup rightName nameMap)

-- TODO May want to remove names for sub-nodes
removeNames :: SgNamedNode -> SyntaxNode
removeNames (_, syntaxNode) = syntaxNode

-- TODO Rename sinks
-- TODO Add unit tests for renameGraph
renameGraph :: SyntaxGraph -> SyntaxGraph
renameGraph (SyntaxGraph nodes edges sinks sources embedMap) =
  SyntaxGraph renamedNodes renamedEdges sinks renamedSources renamedEmbedMap
  where
    (renamedNodes, nameMap, _) = foldl' renameNodeFolder ([], [], 0) $ sortOn removeNames nodes
    renamedEdges = sort $ fmap (renameEdge nameMap) edges
    renamedSources = sort $ fmap (renameSource nameMap) sources
    renamedEmbedMap = sort $ fmap (renameEmbed nameMap) embedMap
  
-- END renameGraph

-- END Unit Test Helpers --


-- 0:(toName "app02",ApplyNode 1)->[]
-- 1:(toName "f0",LiteralNode "f")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "f0") Nothing,NameAndPort (toName "app02") (Just 0))},0)]
-- 2:(toName "x1",LiteralNode "x")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "x1") Nothing,NameAndPort (toName "app02") (Just 2))},0)]
-- 3:(toName "y3",NameNode "y")->[(Edge {edgeOptions = [], edgeEnds = (EndNone,EndNone), edgeConnection = (NameAndPort (toName "y3") Nothing,NameAndPort (toName "app02") (Just 1))},0)]
singleApplyGraph :: FGR.Gr SgNamedNode Edge
singleApplyGraph = syntaxGraphToFglGraph $ translateStringToSyntaxGraph "y = f x"

makeTreeRootTest :: (String, [Maybe SgNamedNode], String) -> Test
makeTreeRootTest (testName, expected, haskellString) = TestCase $ assertEqual testName expected actual where
  actual = fmap (ING.lab graph) treeRoots
  graph = syntaxGraphToFglGraph $ translateStringToSyntaxGraph haskellString
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

applyTests :: Test
applyTests = TestList [
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
  assertEqualSyntaxGraphs [
      "y = f 3 4",
      "y = (f 3) 4"
      ]
  ]

composeApplyTests :: Test
composeApplyTests = TestList [
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
  ]

infixTests :: Test
infixTests = TestList [
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

letTests :: Test
letTests = TestList [
  TestLabel "letTests1" $ assertEqualSyntaxGraphs [
      "y = f 1",
      "y = let x = 1 in f x",
      "y = let {b = a; a = 1} in f b"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = 2",
      "y = let z = 2 in z",
      "y = let {z = 2; z2 = z} in z2"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = f y",
      "y = let x = f x in x"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = f 7 5",
      "y = let {a = 7; b = f a 5} in b"
     ]
  ,
  assertEqualSyntaxGraphs [
      "y x = x",
      "y x = let z = x in z"
      ]
  ,
  assertEqualSyntaxGraphs [
      "fibs = let {y = cons 0 (cons 1 (zipWith (+) y (tail y)))} in y",
      "fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y x = y x",
      "y = let {z = (\\x -> y x)} in z",
      "y = let {z x = y x} in z "
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = f 3 y",
      "y = x where x = f 3 y",
      "y = let x = f 3 y in x"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y x = f x",
      "y x1 = let {x2 = x1; x3 = x2; x4 = f x3} in x4"
      ]
  ,
  -- TODO Fix this test. The second line has two apply icons instead of one.
  -- See VisualTranslateTests/letTests
  -- assertEqualSyntaxGraphs [
  --     "y x1 = (f x1) x1",
  --     "y x1 = let x2 = f x1 in x2 x1"
  --    ]

  assertEqualSyntaxGraphs [
      "y x = 3",
      "y x = let x = 3 in x"
      ]

  -- TODO Fix test. Second line should use compose apply.
  -- See VisualTranslateTests/letTests
  -- assertEqualSyntaxGraphs [
  --     "y = g $ f y",
  --     "y = let {a = f b; b = g a} in b"
  --     ]
  ]

negateTests :: Test
negateTests = TestList [
  assertEqualSyntaxGraphs [
      "y = negate 1",
      "y = -1"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = negate ((/) 1 2)",
      "y = -1/2"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = negate x",
      "y = -x"
      ]
  ]

enumTests :: Test
enumTests = TestList [
  assertEqualSyntaxGraphs [
      "y = enumFrom 1",
      "y = [1..]"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = enumFromThen 1 2",
      "y = [1,2..]"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = enumFromTo 0 10",
      "y = [0..10]"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = enumFromThenTo 0 1 10",
      "y = [0,1..10]"
      ]
  ]

patternTests :: Test
patternTests = TestList [
  -- TODO Remove branch icon
  assertEqualSyntaxGraphs [
      "y (F x) = x",
      "y = (\\(F x) -> x)"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y = let {F x y = 3} in x y",
      "y = let {g = 3; F x y = g} in x y"
      ]
  ]

lambdaTests :: Test
lambdaTests = TestList [
  assertEqualSyntaxGraphs [
      "y x = (\\z -> x)",
      "y = (\\x -> (\\z -> x))"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y x = case x of {0 -> 1; 3 -> 5}",
      "{y 0 = 1; y 3 = 5}"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y p = case p of {F x -> x; G x -> x}",
      "{y (F x) = x; y (G x) = x}"
      ]
  ,
  assertEqualSyntaxGraphs [
      -- TODO Since there are no patterns for z, this should just be "case p of"
      "y p z = case (p, z) of {((F x), z') -> x z'; ((G x), z') -> z' x}",
      "{y (F x) z = x z; y (G x) z = z x}"
      ]
  ,
  assertEqualSyntaxGraphs [
      "y x = f x y",
      "y x = z where z = f x y"
      ]
  ]

-- Yes, the commas get their own line
translateUnitTests :: Test
translateUnitTests = TestList [
  TestLabel "fmapTest" $ assertEqualSyntaxGraphs [
      "y = fmap f x",
      "y = f <$> x"
      ]
  ,
  TestLabel "applyTests" applyTests
  ,
  TestLabel "composeApplyTests" composeApplyTests
  ,
  TestLabel "infixTests" infixTests
  , TestLabel "letTests" letTests
  , TestLabel "negateTests" negateTests
  , TestLabel "enumTests" enumTests
  , TestLabel "patternTests" patternTests
  , TestLabel "lambdaTests" lambdaTests
  ]

allUnitTests :: Test
allUnitTests = TestList[
  TestLabel "collapseUnitTests" collapseUnitTests,
  TestLabel "translateTests" translateUnitTests
  ]
