{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualTranslateTests(
  visualTranslateTests
  ) where

import Diagrams.Prelude hiding ((#), (&))

import qualified Data.Graph.Inductive.Graph as ING
import Data.List(intercalate)

import Types(SpecialQDiagram, SpecialBackend, NodeName(..))
import Translate(translateStringToCollapsedGraphAndDecl
                , translateStringToSyntaxGraph)
import TranslateCore(syntaxGraphToFglGraph, SyntaxGraph(..))
import Rendering(renderIngSyntaxGraph)
import Icons(textBox, TransformParams(..))


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

composeTests :: [String]
composeTests = [
  "y = f (g x)",
  "y = f . g",
  "y = f3 . f2 . f1",
  "y = f3 . f2 . f1 $ x",
  "y = f1 $ f6 (f2 (f3 . f4)) (f5 x)"
  ]

-- | nestedTests / collapseTest
nestedTests :: [String]
nestedTests = [
  "y = f x",
  "y = let x = 1 in f (g x)",
  "y = f []",
  "y = f [1]",
  "y = f [1,2]",
  "y = f [g 3, h 5]",
  "y = f $ g (\\x -> x)",
  "y = f y",
  "y = f (g y)",
  "y = f1 (f2 ( f3 (f4 2))) 1", -- test compose embedded in apply
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
  "y x = case x of {Just w -> (let (z,_) = w in z)}",
  "y = case x of 1 -> f 0",
  "y (Port x) = case x of 0 -> 1",
  "y (x@(Foo y)) = if 0 then x else 1"
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

tupleTests :: [String]
tupleTests = [
  "y = ()",
  "(x, y) = (1,2)",
  "(x, y, z) = (1,2,3)",
  "y = (,x) 2",
  "y = (x,) 2",
  "y = (,,x) 2",
  "y = (,x,) 2",
  "y = (x,,) 2",
  "y = (x,z,) 2",
  "y = (x,,z) 2",
  "y = (,x,z) 2"
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
  "y = case x of {Foo a -> a}",
  "y = case x of {Foo a -> f a; Bar a -> f a}",
  "y = case x of {F x -> x; G x -> x}",
  "y = case x of {F -> 0; G -> 1}",
  "z = case x of {0 -> 1; y -> y}",
  "y x = case f x of {0 -> x; Foo x -> x}"
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

  "y = let {g = 3; F x y = h g} in x y",

  "y = let {F x y = 3} in x y",

  "y = let F x y = g in x y",

  "F x = g x",
  "Foo (Bar x) (Baz y) = f 1 2 x y",
  "Foo x y = f 1 y x",

  "t@(x,y) = (x,y)",
  "y = let {t@(_,_) = (3,4)} in t + 3",

  -- TODO There is no bind text box for n2
  "n1@(n2@(x,y)) = f n1 n2 x y",

  "n0@(Foo n1@(Bar x) n2@(Baz y)) = f n0 n1 x n2 y",
  "baz = case 0 of {n0@(Foo n1@(Bar x) n2@(Baz y)) -> f n0 n1 x n2 y}",
  "func n0@(Foo n1@(Bar x) n2@(Baz y)) = f n0 n1 x n2 y",

  "y = let {(x, y) = (1,2)} in x + y",
  "y = let {(x, y) = (1,2); (z, w) = x; (m, g) = y} in foo x y z w m g",
  "(x:y) = 2",

  -- test labelled ports
  "Foo x1 x2 = 4"
  ]

lambdaTests :: [String]
lambdaTests = [
  "y = (\\x -> (\\x -> (\\x -> x) x) x)",
  "y = (\\x -> (\\x -> (\\x -> x)))",
  "y = (\\y -> y)",
  "y = (\\x1 -> (\\x2 -> (\\x3 -> x1 x2 x3)))",
  "y x = (\\z -> x)",
  "y x = x",
  "y x = y x",
  "y x = g y y",
  "y f x = f x",
  "y x = x y",
  "y x1 x2 = f x1 x3 x2",
  "y x1 x2 = f x1 x2",
  "y x = f x1 x2",
  "y (-1) = 2",
  "y 1 = 0",
  "y x = z 3 where z = f x y"
  ]

letTests :: [String]
letTests = [
  -- TODO fix. See UnitTests/letTests
  "y x = f x x",
  "y x1 = let x2 = f x1 in x2 x1",

  -- TODO fix. See UnitTests/letTests
  "y = g $ f y",
  "y = let {a = f b; b = g a} in b",

  "y = let {a= 1; x = let {a = 27; x = f a 2} in x} in x",
  "y = let {a = b; b = a; d = f a} in d",
  "y = let {a = b; b = a} in a",
  "y = let x = x in x"
  ]

operatorTests :: [String]
operatorTests = [
  -- right section
  "y = map (++ 1) 3"
  ]

otherTests :: [String]
otherTests = [
  "y = f 1 'c' 2.3 \"foobar\"",
  "fact x = if (x == 0) then 1 else (x * fact (x - 1))",
  "fibs = cons 0 (cons 1 (zipWith (+) fibs (tail fibs)))",
  "y x = if x then (if z then q else x) else w",
  "y x1 x2 x3 = if f x1 then g x2 else h x3",
  "y x1 x2 x3 = if x1 then x2 else x3",
  "y = if b then x else n",
  "y2 = f x1 x2 x3 x4",
  "y = x",
  "y x = y x",
  "y = f 3 y",
  "y = f x",
  "y = f (g x1 x2) x3",
  "y = (f x1 x2) (g x1 x2)",
  "y = Foo.bar",
  "y x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10",
  -- test that whitespace is preserved
  "y = \" foo  bar   baz    \""
  ]

testDecls :: [String]
testDecls = mconcat [
  composeTests
  ,nestedTests
  ,doTests
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


translateStringToDrawing :: SpecialBackend b Double =>
  String
  -> IO (SpecialQDiagram b Double)
translateStringToDrawing s = do
  putStrLn $ "Translating string: " ++ s
  let
    (collapsedGraph, decl) = translateStringToCollapsedGraphAndDecl s
    syntaxGraph = translateStringToSyntaxGraph s
    fglGraph = syntaxGraphToFglGraph syntaxGraph
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
  if False then printAction else pure ()  -- Supress unused printAction warning
  renderIngSyntaxGraph collapsedGraph
  -- renderIngSyntaxGraph fglGraph

visualTranslateTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
visualTranslateTests = do
  drawings <- traverse translateStringToDrawing testDecls
  let
    textDrawings
      = fmap
        (\t ->
           alignL $ textBox t (TransformParams (NodeName (-1)) 0 False mempty))
        testDecls
    vCattedDrawings = vsep 1 $ zipWith (===) (fmap alignL drawings) textDrawings
  pure vCattedDrawings
