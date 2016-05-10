{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

-- Note: (#) and (&) are hidden in all Glance source files, since they would require
-- - an special case when translating when Glance is run on its own source code.
import Diagrams.Prelude hiding ((#), (&))
import Diagrams.Backend.SVG.CmdLine
import qualified Language.Haskell.Exts as Exts

import Icons(flatLambda, textBox, colorScheme, ColorStyle(..), nestedApplyDia)
import Rendering(renderDrawing)
import Util(toNames, portToPort, iconToPort, iconToIcon,
  iconToIconEnds, iconTailToPort)
import Types(Icon(..), Drawing(..), EdgeEnd(..))
import Translate(translateString, drawingsFromModule)


-- TODO Now --
-- Fix icon nesting if a non-nestable icon (eg. flatLambdaIcon) is part of the expression.
-- - eg. y = f $ g (\x -> x)
-- Fix rotation missing edges to nested diagrams.

-- Add a maximum nesting depth.
-- Clean up Rendering and Icons.

-- Refactor Translate
-- Add documentation.
-- Have the file be a command line argument to main.

-- Move tests out of main.

-- TODO Later --
-- Why is totalLengthOfLines not nesting?

-- Visual todos:
-- Don't rotate text and nested icons, give them rectangualar bounding boxes in GraphViz. (Perhaps use a typeclass for isRotateAble)
-- Give lines a black border to make line crossings easier to see.
-- Give lines that cross the border of a lambda function a special color.
-- Line intersections should have a small circle. This could probably be done with
-- a line ending.
-- Let each bool, value pair in Guard icon be flipped to reduce line crossings. Do the same for case.
-- Let lines connect to ports in multiple locations (eg. case value, or guard result)
-- Rotate icons based on the outgoing line's difference from ideal angle, not line distance.
-- Improve line routing. Draw curved lines with outgoing lines at fixed angles.
--  - connectPerim might be useful for this.
-- For nested apply, cycle through different colors and line styles (eg. dashed, solid, wavy)
-- - for each nesting level. This will help distinguish what is an argument to which funciton.

-- Translate todos:
-- Make nested version of FlatLambdaIcon
-- Fix test case x of {0 -> 1; y -> y}.
-- Add proper RecConstr, and RecUpdate support.
-- Eliminate BranchIcon in Alts.
-- Eliminate BranchIcon for the identity funciton "y x = x"
-- otherwise Guard special case

(d0A, d0B, d0Res, d0Foo, d0Bar) = ("A", "B", "res", "foo", "bar")
d0Icons = toNames
  [(d0A, ApplyAIcon 1),
  (d0B, ApplyAIcon 1),
  (d0Res, ResultIcon),
  (d0Foo, TextBoxIcon d0Foo),
  (d0Bar, TextBoxIcon d0Bar)
  ]

d0Edges =
  [
  portToPort d0A 0 d0B 1,
  iconToPort d0Foo d0B 0,
  iconToPort d0Res d0A 1,
  iconToPort d0Foo d0B 0,
  iconToPort d0Bar d0B 2,
  iconToPort d0Bar d0A 2
  ]

drawing0 = Drawing d0Icons d0Edges []
d0Name = toName "d0"

(s1Lam, s1y, s1z, s1q) = ("lam0", "y", "z", "q")
superIcons = toNames [
  (s1Lam, LambdaRegionIcon 3 d0Name),
  (s1y, TextBoxIcon s1y),
  (s1z, TextBoxIcon s1z),
  (s1q, TextBoxIcon s1q)
  ]

superEdges =
  [
  portToPort (s1Lam .> d0A) 1 s1Lam 0,
  iconToIcon s1y s1Lam,
  iconToIcon s1z s1Lam,
  iconToIcon s1q s1Lam,
  iconToIcon d0A s1z,
  iconToPort (s1Lam .> d0Foo .> d0Foo) s1Lam 0
  ]

--superDrawing = Drawing [((toName "lam0"), LambdaRegionIcon 3 (toName"d0"))] superEdges [((toName "d0"), drawing0)]
superDrawing = Drawing superIcons superEdges [(d0Name, drawing0)]

super2Icons = toNames [
  (s1Lam, LambdaRegionIcon 1 d0Name),
  --("y", TextBoxIcon "y"),
  ("lam1", LambdaRegionIcon 2 d0Name)
  ]

super2Edges =
  [
  iconToIcon s1Lam "lam1"
  --iconToIcon "y" "lam0"
  ]

super2Drawing = Drawing super2Icons super2Edges [(d0Name, drawing0)]
super2Name = toName "s2"

super3Icons = toNames [
  (s1Lam, LambdaRegionIcon 3 super2Name),
  --("y", TextBoxIcon "y"),
  ("lam1", LambdaRegionIcon 4 super2Name)
  ]

super3Edges =
  [
--  iconToIcon "lam0" "lam1",
  iconToIcon s1Lam "A"
  ]

super3Drawing = Drawing super3Icons super2Edges [(super2Name, super2Drawing)]

(fG0, fOne, fEq0, fMinus1, fEq0Ap, fMinus1Ap, fTimes, fRecurAp, fTimesAp, fArg, fRes) =
  ("g0", "one", "eq0", "-1", "eq0Ap", "-1Ap", "*", "recurAp", "*Ap", "arg", "res")

fact0Icons = toNames
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon fMinus1),
  (fEq0Ap, ApplyAIcon 1),
  (fMinus1Ap, ApplyAIcon 1),
  (fTimes, TextBoxIcon fTimes),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  (fArg, BranchIcon),
  (fRes, ResultIcon)
  ]

fact0Edges = [
    iconToPort fEq0 fEq0Ap 0,
    portToPort fEq0Ap 1 fG0 3,
    iconToPort fMinus1 fMinus1Ap 0,
    iconToPort fTimes fTimesAp 0,
    iconToPort fOne fG0 2,
    portToPort fTimesAp 2 fG0 4,
    portToPort fRecurAp 1 fTimesAp 3,
    iconToPort fArg fEq0Ap 2,
    iconToPort fArg fMinus1Ap 2,
    iconToPort fArg fTimesAp 1,
    portToPort fMinus1Ap 1 fRecurAp 2,
    iconToPort fRes fG0 0
  ]

fact0Drawing = Drawing fact0Icons fact0Edges []
fact0Name = toName "fac0"

factLam0Icons = toNames [
  ("lam0", LambdaRegionIcon 1 fact0Name),
  ("fac", TextBoxIcon "factorial")
  ]

factLam0Edges = [
  iconToPort ("lam0" .> fArg) "lam0" 0,
  iconToPort "lam0" ("lam0" .> fRecurAp) 0,
  iconToIcon "lam0" "fac"
  ]

factLam0Drawing = Drawing factLam0Icons factLam0Edges [(fact0Name, fact0Drawing)]

fact1Icons = toNames
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon fMinus1),
  (fTimes, TextBoxIcon fTimes),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  (fArg, BranchIcon),
  (fRes, ResultIcon)
  ]

fact1Edges = [
  iconToIconEnds fArg EndNone fEq0 EndAp1Arg,
  iconTailToPort fEq0 EndAp1Result fG0 3,
  iconToIconEnds fArg EndNone fMinus1 EndAp1Arg,
  iconTailToPort fMinus1 EndAp1Result fRecurAp 2,
  iconToPort fTimes fTimesAp 0,
  iconToPort fOne fG0 2,
  portToPort fTimesAp 1 fG0 4,
  portToPort fRecurAp 1 fTimesAp 3,
  iconToPort fArg fTimesAp 2,
  iconToPort fRes fG0 0
  ]

fact1Drawing = Drawing fact1Icons fact1Edges []

factLam1Drawing = Drawing factLam0Icons factLam0Edges [(fact0Name, fact1Drawing)]

-- fact2 is like fact1, but uses fTimesAp port 2 to distrubute the argument,
-- not fArg
fact2Icons = toNames
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon fMinus1),
  (fTimes, TextBoxIcon fTimes),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  --(fArg, BranchIcon),
  (fRes, ResultIcon)
  ]

fact2Edges = [
  --iconToIconEnds fArg EndNone fEq0 EndAp1Arg,
  iconTailToPort fEq0 EndAp1Arg fTimesAp 2,
  iconTailToPort fEq0 EndAp1Result fG0 3,
  --iconToIconEnds fArg EndNone fMinus1 EndAp1Arg,
  iconTailToPort fMinus1 EndAp1Arg fTimesAp 2,
  iconTailToPort fMinus1 EndAp1Result fRecurAp 2,
  iconToPort fTimes fTimesAp 0,
  iconToPort fOne fG0 2,
  portToPort fTimesAp 1 fG0 4,
  portToPort fRecurAp 1 fTimesAp 3,
  --iconToPort fArg fTimesAp 2,
  iconToPort fRes fG0 0
  ]

fact2Drawing = Drawing fact2Icons fact2Edges []

factLam2Edges = [
  iconToPort ("lam0" .> fTimesAp .> (2 :: Int)) "lam0" 0,
  iconToPort "lam0" ("lam0" .> fRecurAp) 0,
  iconToIcon "lam0" "fac"
  ]
factLam2Drawing = Drawing factLam0Icons factLam2Edges [(fact0Name, fact2Drawing)]

(arr1, arr2, arr3, arr4) = ("arr1", "arr2", "arr3", "arr4")

arrowTestIcons = toNames [
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

arrowTestDrawing = Drawing arrowTestIcons arrowTestEdges []

nestedTestIcons = toNames [
  ("n1", NestedApply "N1" args),
  ("t1", TextBoxIcon "T1"),
  ("t2", TextBoxIcon "t2")
  ]
  where
    innerArgs = [Just (toName "t", TextBoxIcon "t"), Nothing, Just (toName "n2", NestedApply "n2" [Nothing])]
    args = [
      Nothing, Just (toName "foo", TextBoxIcon "3"),
      Just (toName "in", NestedApply "inner" innerArgs)
      ]

nestedTestEdges = [
  iconToPort "t1" "n1" 2,
  --iconToPort "t1" "in" 1,
  --iconToPort "t2" ("n1" .> "in") 3,
  iconToPort "t2" ("n1" .> "in" .> "n2") 2
  ]

nestedTextDrawing = Drawing nestedTestIcons nestedTestEdges []

main1 :: IO ()
main1 = do
  placedNodes <- renderDrawing nestedTextDrawing
  mainWith (bgFrame 1 (backgroundC colorScheme) placedNodes :: Diagram B)

main2 = mainWith ((bgFrame 0.1 black $ dia False 0)  :: Diagram B)
  where
    args = [Nothing, Just (toName "foo", TextBoxIcon "3"), Just (toName "in", NestedApply "inner" [Just (toName "t", TextBoxIcon "t")])]
    dia = nestedApplyDia "Hello world" args

main3 :: IO ()
main3 = do
  renderedDiagrams <- traverse renderDrawing allDrawings
  let vCattedDrawings = vsep 0.5 renderedDiagrams
  mainWith (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)
  where
    allDrawings = [
      drawing0,
      superDrawing,
      super2Drawing,
      super3Drawing,
      fact0Drawing,
      factLam0Drawing,
      fact1Drawing,
      factLam1Drawing,
      fact2Drawing,
      factLam2Drawing,
      arrowTestDrawing
      ]

nestedTests = [
  "y = f x",
  "y = f (g x)",
  "y = let x = 1 in f x",
  "y = let x = 1 in f (g x)",
  "y = f []",
  "y = f [1]",
  "y = f [1,2]",
  "y = f [g 3, h 5]"
  ]

dollarTests = [
  "y = f $ g 3",
  " y = f 1 $ g 2 "
  ]

specialTests = [
  "lookupTail EndAp1Arg = (arrowTail .~ dart')",
  "y = x .~ y",
  "initialIdState = IDState 0",
  "y = f x",
  "yyy = fff xxx",
  "yyyyy = fffff xxxxx"
  ]

negateTests = [
  "y = -1",
  "y = -1/2",
  "y = -x"
  ]

doTests = [
  "y = do {x1}",
  "y = do {x1; x2}",
  "y = do {x1; x2; x3}",
  "y = do {x1 <- m1; x2}",
  "y = do {(x1, x2) <- m1; x1 + x2}",
  "y = do {x1 <- m1; x2 <- f x1; g x2}",
  "y = do {let {x = 1}; x2 <- x; f x2}"
  ]

enumTests = [
  "y = [1..]",
  "y = [1,2..]",
  "y = [0..10]",
  "y = [0,1..10]"
  ]

tupleTests = [
  "y = ()",
  "(x, y) = (1,2)",
  "(x, y, z) = (1,2,3)"
  ]

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

caseTests = [
  "y = case x of {0 -> 1; 2 -> 3}",
  "y = case f x of {0 -> 1; 2 -> 3}",
  "y = case x of {Foo a -> a}",
  "y = case x of {Foo a -> f a; Bar a -> f a}",
  "y = case x of {F x -> x; G x -> x}",
  "y = case x of {F -> 0; G -> 1}",
  "z = case x of {0 -> 1; y -> y}"
  ]

guardTests = [
  "y x\n\
  \  | x == 0 = 1",
  "y x\n\
  \  | x == 0 = 1\n\
  \  | otherwise = 2"
  ]

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

operatorTests = [
  "y = 1 + 2",
  "y = map (1 ++) 3",
  "y = map (++ 1) 3"
  ]

otherTests = [
  "y = f 1 'c' 2.3 \"foobar\"",
  "fact x = if (x == 0) then 1 else (fact x (x - 1))",
  "fact x = if ((==) 0 x) then 1 else (fact x ((-) x 1))",
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

testDecls = mconcat [
  --dollarTests
  nestedTests
  -- ,negateTests
  -- ,doTests
  -- ,enumTests
  -- ,caseTests
  -- ,lambdaTests
  -- ,guardTests
  -- ,patternTests
  -- ,specialTests
  -- ,tupleTests
  -- ,listTests
  -- ,letTests
  -- ,operatorTests
  -- ,otherTests
  ]

translateStringToDrawing :: String -> IO (Diagram B)
translateStringToDrawing s = do
  let
    (drawing, decl) = translateString s
  print decl
  putStr "\n"
  print drawing
  putStr "\n\n"
  renderDrawing drawing

main4 :: IO ()
main4 = do
  drawings <- traverse translateStringToDrawing testDecls
  let
    textDrawings = fmap (\t -> alignL $ textBox t False 0) testDecls
    vCattedDrawings = vsep 1 $ zipWith (===) (fmap alignL drawings) textDrawings
  mainWith (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)

testFiles = [
  "./app/Main.hs",
  "./test/test_translate.hs"
  ]

main5 :: IO ()
main5 = do
  parseResult <- Exts.parseFileWithExts [Exts.EnableExtension Exts.MultiParamTypeClasses, Exts.EnableExtension Exts.FlexibleContexts]
    --"./app/Icons.hs"
    "./test/test_nesting.hs"
  let
    parsedModule = Exts.fromParseResult parseResult
    drawings = drawingsFromModule parsedModule
  print parsedModule
  print "\n\n"
  --print drawings

  diagrams <- traverse renderDrawing drawings
  let
    vCattedDrawings = vsep 1 $ fmap alignL diagrams
  mainWith (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)


main :: IO ()
main = main5
