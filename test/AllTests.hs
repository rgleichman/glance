import Prelude hiding (return)
import Diagrams.Prelude hiding ((#), (&))
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.TwoD.GraphViz as DiaGV
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA

import Data.List(intercalate)
import qualified Data.Graph.Inductive.Graph as ING
import qualified Data.Graph.Inductive.PatriciaTree as FGR

import Test.HUnit

import Icons(textBox, colorScheme, ColorStyle(..), coloredTextBox)
import Rendering(renderDrawing, customLayoutParams, renderIngSyntaxGraph)
import Util(portToPort, iconToPort,
  iconToIconEnds, iconTailToPort)
import Types(Icon(..), Drawing(..), EdgeEnd(..), SgNamedNode, Edge(..), SyntaxNode(..), NameAndPort(..),
             IngSyntaxGraph, NodeName(..), Port(..), LikeApplyFlavor(..))
import Translate(translateString, stringToSyntaxGraph)
import TranslateCore(syntaxGraphToFglGraph, SyntaxGraph(..))
import GraphAlgorithms(collapseNodes)
import qualified GraphAlgorithms

iconToIntPort :: NodeName -> NodeName -> Int -> Edge
iconToIntPort x y p = iconToPort x y (Port p)

intPortToPort :: NodeName -> Int -> NodeName -> Int -> Edge
intPortToPort x1 port1 x2 port2 = portToPort x1 (Port port1) x2 (Port port2)

drawing0 :: Drawing
drawing0 = Drawing d0Icons d0Edges where
  [d0A, d0B, d0Res, d0Foo, d0Bar] = fmap NodeName [0..4] --["A", "B", "res", "foo", "bar"]
  d0Icons =
    [(d0A, ApplyAIcon 1),
     (d0B, ApplyAIcon 1),
     (d0Res, ResultIcon),
     (d0Foo, TextBoxIcon "foo"),
     (d0Bar, TextBoxIcon "bar")
    ]
  d0Edges =
    [
      intPortToPort d0A 0 d0B 1,
      iconToIntPort d0Foo d0B 0,
      iconToIntPort d0Res d0A 1,
      iconToIntPort d0Foo d0B 0,
      iconToIntPort d0Bar d0B 2,
      iconToIntPort d0Bar d0A 2
    ]
    

fG0, fOne, fEq0, fMinus1, fEq0Ap, fMinus1Ap, fTimes, fRecurAp, fTimesAp, fArg, fRes :: NodeName
[fG0, fOne, fEq0, fMinus1, fEq0Ap, fMinus1Ap, fTimes, fRecurAp, fTimesAp, fArg, fRes] =
  fmap NodeName [0..10]
--  ["g0", "one", "eq0", "-1", "eq0Ap", "-1Ap", "*", "recurAp", "*Ap", "arg", "res"]

fact0Drawing :: Drawing
fact0Drawing = Drawing fact0Icons fact0Edges where
  fact0Icons =
    [
      (fG0, GuardIcon 2),
      (fOne, TextBoxIcon "1"),
      (fEq0, TextBoxIcon "== 0"),
      (fMinus1, TextBoxIcon "-1"),
      (fEq0Ap, ApplyAIcon 1),
      (fMinus1Ap, ApplyAIcon 1),
      (fTimes, TextBoxIcon "*"),
      (fRecurAp, ApplyAIcon 1),
      (fTimesAp, ApplyAIcon 2),
      (fArg, BranchIcon),
      (fRes, ResultIcon)
    ]
  fact0Edges = [
    iconToIntPort fEq0 fEq0Ap 0,
    intPortToPort fEq0Ap 1 fG0 3,
    iconToIntPort fMinus1 fMinus1Ap 0,
    iconToIntPort fTimes fTimesAp 0,
    iconToIntPort fOne fG0 2,
    intPortToPort fTimesAp 2 fG0 4,
    intPortToPort fRecurAp 1 fTimesAp 3,
    iconToIntPort fArg fEq0Ap 2,
    iconToIntPort fArg fMinus1Ap 2,
    iconToIntPort fArg fTimesAp 1,
    intPortToPort fMinus1Ap 1 fRecurAp 2,
    iconToIntPort fRes fG0 0
    ]


fact1Icons :: [(NodeName, Icon)]
fact1Icons =
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon "-1"),
  (fTimes, TextBoxIcon "*"),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  (fArg, BranchIcon),
  (fRes, ResultIcon)
  ]

fact1Edges :: [Edge]
fact1Edges = [
  iconToIconEnds fArg EndNone fEq0 EndAp1Arg,
  iconTailToPort fEq0 EndAp1Result fG0 (Port 3),
  iconToIconEnds fArg EndNone fMinus1 EndAp1Arg,
  iconTailToPort fMinus1 EndAp1Result fRecurAp (Port 2),
  iconToIntPort fTimes fTimesAp 0,
  iconToIntPort fOne fG0 2,
  intPortToPort fTimesAp 1 fG0 4,
  intPortToPort fRecurAp 1 fTimesAp 3,
  iconToIntPort fArg fTimesAp 2,
  iconToIntPort fRes fG0 0
  ]

fact1Drawing :: Drawing
fact1Drawing = Drawing fact1Icons fact1Edges

-- fact2 is like fact1, but uses fTimesAp port 2 to distrubute the argument,
-- not fArg
fact2Icons :: [(NodeName, Icon)]
fact2Icons =
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon "-1"),
  (fTimes, TextBoxIcon "*"),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  --(fArg, BranchIcon),
  (fRes, ResultIcon)
  ]

fact2Edges :: [Edge]
fact2Edges = [
  --iconToIconEnds fArg EndNone fEq0 EndAp1Arg,
  iconTailToPort fEq0 EndAp1Arg fTimesAp (Port 2),
  iconTailToPort fEq0 EndAp1Result fG0 (Port 3),
  --iconToIconEnds fArg EndNone fMinus1 EndAp1Arg,
  iconTailToPort fMinus1 EndAp1Arg fTimesAp (Port 2),
  iconTailToPort fMinus1 EndAp1Result fRecurAp (Port 2),
  iconToIntPort fTimes fTimesAp 0,
  iconToIntPort fOne fG0 2,
  intPortToPort fTimesAp 1 fG0 4,
  intPortToPort fRecurAp 1 fTimesAp 3,
  --iconToIntPort fArg fTimesAp 2,
  iconToIntPort fRes fG0 0
  ]

fact2Drawing :: Drawing
fact2Drawing = Drawing fact2Icons fact2Edges

arrowTestDrawing :: Drawing
arrowTestDrawing = Drawing arrowTestIcons arrowTestEdges where
  [arr1, arr2, arr3, arr4] = fmap NodeName [0..3] --["arr1", "arr2", "arr3", "arr4"]
  arrowTestIcons = [
    (arr1, TextBoxIcon "1"),
    (arr2, TextBoxIcon "2"),
    (arr3, TextBoxIcon "3"),
    (arr4, TextBoxIcon "4")
    ]
  arrowTestEdges = [
    iconToIconEnds arr1 EndAp1Arg arr2 EndAp1Result,
    iconToIconEnds arr1 EndAp1Result arr3 EndAp1Arg,
    iconToIconEnds arr2 EndAp1Result arr3 EndAp1Result,
    iconToIconEnds arr1 EndAp1Arg arr4 EndAp1Arg
    ]
    

nestedTextDrawing :: Drawing
nestedTextDrawing = Drawing nestedTestIcons nestedTestEdges where
  [n1, t1, t2, inner, t, n2, n3, foo, in1, n4] = fmap NodeName [0..9]
  nestedTestIcons = [
    (n1, NestedApply ApplyNodeFlavor args),
    (t1, TextBoxIcon "T1"),
    (t2, TextBoxIcon "t2")
    ]
    where
      innerArgs = [
        Just (inner, TextBoxIcon "inner"),
        Just (t, TextBoxIcon "t"),
        Nothing,
        Just (n2, NestedApply ApplyNodeFlavor [Just (n4, TextBoxIcon "N4"), Nothing])
        ]
      args = [
        Just (n3, TextBoxIcon "n3"),
        Nothing,
        Just (foo, TextBoxIcon "3"),
        Just (in1, NestedApply ApplyNodeFlavor innerArgs)
        ]
  nestedTestEdges = [
    iconToIntPort t1 n1 2,
    --iconToIntPort "t1" "in" 1,
    --iconToIntPort "t2" ("n1" .> "in") 3,
    --iconToIntPort "t2" ("n1" .> "in" .> "n2") 2
    -- TODO This edge is not drawn currently. See todo in drawingToIconGraph in Rendering.
    iconToIntPort t2 n2 2
    ]
  
renderTests :: IO (Diagram B)
renderTests = do
  renderedDiagrams <- traverse renderDrawing allDrawings
  let vCattedDrawings = vsep 0.5 renderedDiagrams
  pure vCattedDrawings
  where
    allDrawings = [
      drawing0,
      fact0Drawing,
      fact1Drawing,
      fact2Drawing,
      arrowTestDrawing,
      nestedTextDrawing
      -- TODO Add a nested test where the function expression is nested.
      ]

composeTests :: [String]
composeTests = [
  "y = f (g x)",
  "y = f . g",
  "y = f . g $ x",
  "y = (f . g) x",
  "y = f3 . f2 . f1",
  "y = f3 . f2 . f1 $ x",
  "y = (f3 . f2 . f1) x",
  "y = f1 $ f6 (f2 (f3 . f4)) (f5 x)"
  ]

-- | nestedTests / collapseTest
nestedTests :: [String]
nestedTests = [
  "y = f x",
  "y = f (g x)",
  "y = let x = 1 in f x",
  "y = let x = 1 in f (g x)",
  "y = f []",
  "y = f [1]",
  "y = f [1,2]",
  "y = f [g 3, h 5]",
  "y = f $ g (\\x -> x)",
  "y = (f 3) 4",
  "y = f y",
  "y = f (g y)",
  "y = f1 (f2 ( f3 (f4 2))) 1", -- test compose embedded in apply
  "y = f1 (f2 $ f3 $ f4 2) 1", -- test compose embedded in apply
  "y = f0 $ f1 $ f2 z (f4 $ f5 q) $ x", -- compose embedded in compose
  "fibs = cons 1 (zipWith (+) fibs (tail fibs))",
  "y = foo (3 + bazOf2) bazOf2 where bazOf2 = baz 2",
  "y = foo (3 + bazOf2) (8 * bazOf2) where bazOf2 = baz 2",
  "Foo x = 1",
  "Foo 1 x = 2",
  "Foo (Bar x) = 1",
  "Foo (Bar (Baz x)) = 1",
  "Foo (Bar (Baz (Foot x))) = 1",
  "Foo (Bar x) (Baz y) = 1",
  "Foo (Bar x) = f 2",
  "Foo (Bar x) = f x",
  "y x = case x of {Just w -> (let (z,_) = w in z)}"
  ]

dollarTests :: [String]
dollarTests = [
  "y = f $ g 3",
  "y = f 1 $ g 2 ",
  "y = f <$> x"
  ]

specialTests :: [String]
specialTests = [
  "lookupTail EndAp1Arg = (arrowTail .~ dart')",
  "y = x .~ y",
  "initialIdState = IDState 0",
  "y = f x",
  "yyy = fff xxx",
  "yyyyy = fffff xxxxx"
  ]

negateTests :: [String]
negateTests = [
  "y = -1",
  "y = -1/2",
  "y = -x"
  ]

doTests :: [String]
doTests = [
  "y = do {x1}",
  "y = do {x1; x2}",
  "y = do {x1; x2; x3}",
  "y = do {x1 <- m1; x2}",
  "y = do {(x1, x2) <- m1; x1 + x2}",
  "y = do {x1 <- m1; x2 <- f x1; g x2}",
  "y = do {let {x = 1}; x2 <- x; f x2}"
  ]

enumTests :: [String]
enumTests = [
  "y = [1..]",
  "y = [1,2..]",
  "y = [0..10]",
  "y = [0,1..10]"
  ]

tupleTests :: [String]
tupleTests = [
  "y = ()",
  "(x, y) = (1,2)",
  "(x, y, z) = (1,2,3)"
  ]

listTests :: [String]
listTests = [
  "y = []",
  "y = [1]",
  "y = [1,2]",
  "y = [1,2,3]",
  "[x] = 1",
  "[x, y] = 2",
  "[x, y, z] = 3"
  -- TODO: Add this test "(x:y) = 3"
  ]

caseTests :: [String]
caseTests = [
  "y = case x of {0 -> 1; 2 -> 3}",
  "y = case f x of {0 -> 1; 2 -> 3}",
  -- TODO Remove the branch icon
  "y = case x of {Foo a -> a}",
  "y = case x of {Foo a -> f a; Bar a -> f a}",
  "y = case x of {F x -> x; G x -> x}",
  "y = case x of {F -> 0; G -> 1}",
  "z = case x of {0 -> 1; y -> y}"
  ]

guardTests :: [String]
guardTests = [
  "y x\n\
  \  | x == 0 = 1",
  "y x\n\
  \  | x == 0 = 1\n\
  \  | otherwise = 2"
  ]

patternTests :: [String]
patternTests = [
  "Foo _ x = 3",
  "y (F x) = x",
  "y = (\\(F x) -> x)",
  "y = let {g = 3; F x y = h g} in x y",
  "y = let {F x y = 3} in x y",
  "y = let {g = 3; F x y = g} in x y",
  "y = let F x y = g in x y",
  "F x = g x",
  "Foo (Bar x) (Baz y) = f 1 2 x y",
  "Foo x y = f 1 y x",
  "t@(x,y) = (x,y)",
  "y = let {t@(_,_) = (3,4)} in t + 3",
  "y = let {(x, y) = (1,2)} in x + y",
  -- TODO: Fix so that lines between patterns are Pattern Color.
  "y = let {(x, y) = (1,2); (z, w) = x; (m, g) = y} in foo x y z w m g",
  "(x:y) = 2"
  ]

lambdaTests :: [String]
lambdaTests = [
  "y = (\\x -> (\\x -> (\\x -> x) x) x)",
  "y = (\\x -> (\\x -> (\\x -> x)))",
  "y = (\\y -> y)",
  "y = (\\x1 -> (\\x2 -> (\\x3 -> x1 x2 x3)))",
  "y x = (\\z -> x)",
  "y = (\\x -> (\\z -> x))",
  "y x = x",
  "y x = y x",
  "y x = g y y",
  "y f x = f x",
  "y x = x y",
  "y x1 x2 = f x1 x3 x2",
  "y x1 x2 = f x1 x2",
  "y x = f x1 x2",
  "{y 0 = 1; y 1= 0}",
  "y (-1) = 2",
  "y 1 = 0",
  "{y (F x) = x; y (G x) = x}",
  "{y (F x) z = x z; y (G x) z = z x}",
  "y x = z 3 where z = f x y",
  "y x = z where z = f x y"
  ]

letTests :: [String]
letTests = [
  "y = let {z = (\\x -> y x)} in z",
  "y = let {z x = y x} in z ",
  "y = x where x = f 3 y",
  "y x1 = let {x2 = x1; x3 = x2; x4 = f x3} in x4",
  "y x1 = let x2 = f x1 in x2 x1",
  "y x = let x = 3 in x",
  "y = let {a= 1; x = let {a = 27; x = f a 2} in x} in x",
  "y = let {a = b; b = a; d = f a} in d",
  "y = let {a = b; b = a} in a",
  "y = let x = x in x",
  -- TODO fix the lack of embedding.
  "y = let {fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))} in fibs",
  "fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))",
  "y = let x = f x in x",
  "y = f y",
  "y = let {a = f b; b = g a} in b",
  "y = let {a = 48; b = a + 3} in b",
  "y = let {b = a; a = 84} in f b",
  "y = let {x = 1} in f x",
  "y = let z = 2 in z",
  "y = let {z = 3; z2 = z} in z2",
  "y x = let z = x in z"
  ]

operatorTests :: [String]
operatorTests = [
  "y = 1 + 2",
  "y = map (1 ++) 3",
  "y = map (++ 1) 3"
  ]

otherTests :: [String]
otherTests = [
  "y = f 1 'c' 2.3 \"foobar\"",
  "fact x = if (x == 0) then 1 else (x * fact (x - 1))",
  "fact x = if ((==) 0 x) then 1 else (x * fact ((-) x 1))",
  "y x = if x then (if z then q else x) else w",
  "y x1 x2 x3 = if f x1 then g x2 else h x3",
  "y x1 x2 x3 = if x1 then x2 else x3",
  "y = if b then x else n",
  "y2 = f x1 x2 x3 x4",
  "y = x",
  "y = f x",
  "y = f (g x)",
  "y = f (g x1 x2) x3",
  "y = (f x1 x2) (g x1 x2)",
  "y = Foo.bar"
  ]

testDecls :: [String]
testDecls = mconcat [
  dollarTests
  ,composeTests
  ,nestedTests
  ,negateTests
  ,doTests
  ,enumTests
  ,caseTests
  ,lambdaTests
  ,guardTests
  ,patternTests
  ,specialTests
  ,tupleTests
  ,listTests
  ,letTests
  ,operatorTests
  ,otherTests
  ]


translateStringToDrawing :: String -> IO (Diagram B)
translateStringToDrawing s = do
  putStrLn $ "Translating string: " ++ s
  let
    (drawing, decl) = translateString s
    syntaxGraph = stringToSyntaxGraph s
    fglGraph = syntaxGraphToFglGraph syntaxGraph
    collapsedGraph = collapseNodes fglGraph
  let
    printAction = do
      print decl
      putStr "\nSyntax Graph:\n"
      putStrLn $ prettyShowSyntaxGraph syntaxGraph
      putStr "\nFGL Graph:\n"
      ING.prettyPrint fglGraph
      putStr "\nCollapsed Graph:\n"
      print collapsedGraph
      putStr "\n\n"
  --printAction
  renderIngSyntaxGraph drawing

translateTests :: IO (Diagram B)
translateTests = do
  drawings <- traverse translateStringToDrawing testDecls
  let
    textDrawings = fmap (\t -> alignL $ textBox t (NodeName (-1)) 0 False mempty) testDecls
    vCattedDrawings = vsep 1 $ zipWith (===) (fmap alignL drawings) textDrawings
  pure vCattedDrawings

-- TODO Remove graphTests
graphTests :: IO (Diagram B)
graphTests = do
  layedOutGraph <- DiaGV.layoutGraph GVA.Neato fglGraph
  pure $ DiaGV.drawGraph
    nodeFunc
    (\_ _ _ _ _ p -> lc white $ stroke p)
    layedOutGraph
  where
    fglGraph = syntaxGraphToFglGraph $ stringToSyntaxGraph "y = f x"
    nodeFunc (_, syntaxNode) =
      place (coloredTextBox white (opaque white) (show syntaxNode) :: Diagram B)

prettyShowList :: Show a => [a] -> String
prettyShowList ls = intercalate "\n" $ fmap show ls

prettyShowSyntaxGraph :: SyntaxGraph -> String
prettyShowSyntaxGraph (SyntaxGraph nodes edges sinks sources _) =
  "SyntaxGraph nodes:\n" ++
  prettyShowList nodes ++
  "\nSyntaxGraph edges:\n" ++
  prettyShowList edges ++
  "\nSyntaxGRaph sinks:\n" ++
  prettyShowList sinks ++
  "\nSyntaxGraph sources:\n" ++
  prettyShowList sources

prettyPrintSyntaxNode :: SyntaxNode -> String
prettyPrintSyntaxNode (NestedApplyNode _ _ namedNodesAndEdges) = concatMap printNameAndEdge namedNodesAndEdges
  where
    printNameAndEdge (namedNode, edge) = "(" ++ prettyPrintNamedNode namedNode ++ "," ++ printEdge edge ++ ")"
    prettyPrintNamedNode = show. fst --  "(" ++ show name ++ "," ++ prettyPrintSyntaxNode syntaxNode ++ ")"
    printEdge (Edge _ _ (NameAndPort n1 _, NameAndPort n2 _)) = show (n1, n2)
prettyPrintSyntaxNode x = show x

renderFglGraph :: FGR.Gr SgNamedNode Edge -> IO (Diagram B)
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
      place (coloredTextBox white (opaque white) (show name ++ prettyPrintSyntaxNode syntaxNode) :: Diagram B) (scaleFactor *^ point)
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

makeCollapseTest :: String -> IO (Diagram B)
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
    fglGraph = syntaxGraphToFglGraph $ stringToSyntaxGraph str
    collapsedGraph = collapseNodes fglGraph
    customTextBox = coloredTextBox white (opaque lime)
    expressionText = alignL $ coloredTextBox white (opaque yellow) str :: Diagram B
    beforeText = alignL $ customTextBox "Before:" :: Diagram B
    afterText = alignL $ customTextBox "After:" :: Diagram B

-- TODO Make this work for many input strings
collapseTests :: IO (Diagram B)
collapseTests = do
  drawings <- traverse makeCollapseTest collapseTestStrings
  pure $ vsep 1 drawings

drawingsAndNames :: [(String, IO (Diagram B))]
drawingsAndNames = [
  ("translate-tests", translateTests),
  ("render-tests", renderTests),
  ("graph-tests", graphTests),
  ("collapse-tests", collapseTests)
  ]

renderDrawings :: [(String, IO (Diagram B))] -> IO ()
renderDrawings = mapM_ saveDrawing where
  saveDrawing (name, drawingMaker) = do
    dia <- drawingMaker
    -- TODO Replace string concatenation with proper path manipulation functions.
    renderSVG ("test/test-output/" ++ name ++ ".svg") (mkWidth 700) (bgFrame 1 (backgroundC colorScheme) dia)

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

fmapTest :: Test
fmapTest = TestCase $ assertEqual "fmapTest" fmap1 fmap2 where
  fmap1 = stringToSyntaxGraph "y = fmap f x"
  fmap2 = stringToSyntaxGraph "y = f <$> x"

translateUnitTests :: Test
translateUnitTests = TestList [
  TestLabel "fmapTest" fmapTest
  ]

allUnitTests :: Test
allUnitTests = TestList[
  TestLabel "collapseUnitTests" collapseUnitTests,
  TestLabel "translateTests" translateUnitTests
  ]

main :: IO ()
--main = print "Hello world"
main = do
--  ING.prettyPrint singleApplyGraph
  renderDrawings drawingsAndNames
  _ <- runTestTT allUnitTests
  pure ()
