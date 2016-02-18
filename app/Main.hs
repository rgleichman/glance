{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Icons(apply0NDia, colorScheme, ColorStyle(..))
import Rendering(renderDrawing)
import Util(toNames, portToPort, iconToPort, iconToIcon,
  iconToIconEnds, iconTailToPort)
import Types(Icon(..), Drawing(..), EdgeEnd(..))
import Translate(translateString)

-- TODO Now --
-- Use getUniqueName
-- Unique names for evalMatch.
-- Handle duplicate names correctly.

-- TODO Later --
-- Eliminate BranchIcon for the identity funciton "y x = x"
-- Let lines connect to ports in multiple locations (eg. argument for Apply0Dia)
-- Add a small black border to lines to help distinguish line crossings.
-- todo: Find out how to hide unqualified names such that recursive drawings are connected correctly
-- todo: Find out and fix why connectinos to sub-icons need to be qualified twice (eg. "lam0" .> "arg" .> "arg")
-- todo: Rotate based on difference from ideal tangent angle, not line distance.
-- todo: Try using connectPerim for port to port connections. Hopefully this will draw a spline.
-- todo: layout and rotate considering external connections.

(d0A, d0B, d0Res, d0Foo, d0Bar) = ("A", "B", "res", "foo", "bar")
d0Icons = toNames
  [(d0A, Apply0NIcon 1),
  (d0B, Apply0NIcon 1),
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
  (fEq0Ap, Apply0NIcon 1),
  (fMinus1Ap, Apply0NIcon 1),
  (fTimes, TextBoxIcon fTimes),
  (fRecurAp, Apply0NIcon 1),
  (fTimesAp, Apply0NIcon 2),
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
  (fRecurAp, Apply0NIcon 1),
  (fTimesAp, Apply0NIcon 2),
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
  (fRecurAp, Apply0NIcon 1),
  (fTimesAp, Apply0NIcon 2),
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

main1 :: IO ()
main1 = do
  placedNodes <- renderDrawing factLam0Drawing
  mainWith ((placedNodes # bgFrame 1 (backgroundC colorScheme)) :: Diagram B)

main2 = mainWith ((apply0NDia 3 # bgFrame 0.1 black)  :: Diagram B)

main3 :: IO ()
main3 = do
  renderedDiagrams <- mapM renderDrawing allDrawings
  let vCattedDrawings = vcat' (with & sep .~ 0.5) renderedDiagrams
  mainWith ((vCattedDrawings # bgFrame 1 (backgroundC colorScheme)) :: Diagram B)
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

testDecls = [
  "y = (\\x -> (\\x -> (\\x -> x) x) x)",
  "y = (\\x -> (\\x -> (\\x -> x)))",
  "y = (\\y -> y)",
  "y = (\\x1 -> (\\x2 -> (\\x3 -> x1 x2 x3)))",
  "y x = (\\z -> x)",
  "y = (\\x -> (\\z -> x))",
  "y x = x"
  -- "y x = y x",
  -- "y x = g y y",
  -- "y f x = f x",
  -- "y x = x y"
  -- "y x1 x2 = f x1 x3 x2",
  -- "y x1 x2 = f x1 x2",
  -- "y x = f x1 x2",
  -- "y2 = f x1 x2 x3 x4",
  -- "y = x",
  -- "y = f x",
  -- "y = f (g x)",
  -- "y = f (g x1 x2) x3",
  -- "y = (f x1 x2) (g x1 x2)"
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
  drawings <- mapM translateStringToDrawing testDecls
  let vCattedDrawings = vcat' (with & sep .~ 0.5) drawings
  mainWith ((vCattedDrawings # bgFrame 1 (backgroundC colorScheme)) :: Diagram B)

main :: IO ()
main = main4
