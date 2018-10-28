{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualRenderingTests (
  renderTests
  ) where

import Diagrams.Prelude hiding ((#), (&))

import Rendering (renderDrawing)
import Types (NodeName(..), Drawing(..), Edge, Icon(..), Port(..), EdgeEnd(..),
              LikeApplyFlavor(..), SpecialQDiagram, SpecialBackend, NamedIcon(..))
import Util(portToPort, iconToPort,
            iconToIconEnds, iconTailToPort, tupleToNamedIcon)

iconToIntPort :: NodeName -> NodeName -> Int -> Edge
iconToIntPort x y p = iconToPort x y (Port p)

intPortToPort :: NodeName -> Int -> NodeName -> Int -> Edge
intPortToPort x1 port1 x2 port2 = portToPort x1 (Port port1) x2 (Port port2)

drawing0 :: Drawing
drawing0 = Drawing d0Icons d0Edges where
  [d0A, d0B, d0Res, d0Foo, d0Bar] = fmap NodeName [0..4] --["A", "B", "res", "foo", "bar"]
  d0Icons = fmap tupleToNamedIcon
    [(d0A, ApplyAIcon 1),
     (d0B, ApplyAIcon 1),
     (d0Res, CaseResultIcon),
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
  fact0Icons = fmap tupleToNamedIcon
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
      -- (fArg, BranchIcon),
      (fRes, CaseResultIcon)
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


fact1Icons :: [NamedIcon]
fact1Icons = fmap tupleToNamedIcon
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon "-1"),
  (fTimes, TextBoxIcon "*"),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  -- (fArg, BranchIcon),
  (fRes, CaseResultIcon)
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
fact2Icons :: [NamedIcon]
fact2Icons = fmap tupleToNamedIcon
  [
  (fG0, GuardIcon 2),
  (fOne, TextBoxIcon "1"),
  (fEq0, TextBoxIcon "== 0"),
  (fMinus1, TextBoxIcon "-1"),
  (fTimes, TextBoxIcon "*"),
  (fRecurAp, ApplyAIcon 1),
  (fTimesAp, ApplyAIcon 2),
  --(fArg, BranchIcon),
  (fRes, CaseResultIcon)
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
  arrowTestIcons = fmap tupleToNamedIcon [
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
  nestedTestIcons = fmap tupleToNamedIcon [
    (n1, NestedApply ApplyNodeFlavor args),
    (t1, TextBoxIcon "T1"),
    (t2, TextBoxIcon "t2")
    ]
    where
      innerArgs = fmap (fmap tupleToNamedIcon) [
        Just (inner, TextBoxIcon "inner"),
        Just (t, TextBoxIcon "t"),
        Nothing,
        Just (n2,
              NestedApply
              ApplyNodeFlavor
              (fmap (fmap tupleToNamedIcon) [Just (n4, TextBoxIcon "N4"), Nothing]))
        ]
      args = fmap (fmap tupleToNamedIcon) [
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

-- TODO refactor these Drawings
nestedCaseDrawing :: Drawing
nestedCaseDrawing = Drawing icons [] where
  [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] = fmap NodeName [0..9]
  icons = fmap tupleToNamedIcon [
    (n0, NestedCaseIcon [Nothing, Nothing, Nothing]),
    (n1, NestedCaseIcon [Nothing, Just $ NamedIcon n2 (TextBoxIcon "n2"), Nothing]),
    (n3, NestedCaseIcon [Nothing, Nothing, Just $ NamedIcon n4 (TextBoxIcon "n4")]),
    (n5, NestedCaseIcon [Nothing,
                         Just $ NamedIcon n6 (TextBoxIcon "n6"),
                         Just $ NamedIcon n7 (TextBoxIcon "n7"),
                         Just $ NamedIcon n8 (TextBoxIcon "n8"),
                         Just $ NamedIcon n9 (TextBoxIcon "n9")])
    ]

nestedGuardDrawing :: Drawing
nestedGuardDrawing = Drawing icons edges where
  [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10] = fmap NodeName [0..10]
  icons = fmap tupleToNamedIcon [
    (n10, TextBoxIcon "n10"),
    (n0, NestedGuardIcon [Nothing, Nothing, Nothing]),
    (n1, NestedGuardIcon [Nothing, Just $ NamedIcon n2 (TextBoxIcon "n2"), Nothing]),
    (n3, NestedGuardIcon [Nothing, Nothing, Just $ NamedIcon n4 (TextBoxIcon "n4")]),
    (n5, NestedGuardIcon [Nothing,
                         Just $ NamedIcon n6 (TextBoxIcon "n6"),
                         Just $ NamedIcon n7 (TextBoxIcon "n7"),
                         Just $ NamedIcon n8 (TextBoxIcon "n8"),
                         Just $ NamedIcon n9 (TextBoxIcon "n9")])
    ]
  edges = [
    iconToIntPort n10 n5 0
    ]

flatCaseDrawing :: Drawing
flatCaseDrawing = Drawing icons edges where
  icons = fmap tupleToNamedIcon [
    (NodeName 0, CaseIcon 0),
    (NodeName 1, CaseIcon 1),
    (NodeName 2, CaseIcon 2)
    ]
  edges = []

flatGuardDrawing :: Drawing
flatGuardDrawing = Drawing icons edges where
  icons = fmap tupleToNamedIcon [
    (NodeName 1, GuardIcon 1),
    (NodeName 2, GuardIcon 2),
    (NodeName 3, GuardIcon 3)
    ]
  edges = []

--renderTests :: IO (Diagram B)
renderTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
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
      nestedTextDrawing,
      nestedCaseDrawing,
      nestedGuardDrawing,
      flatCaseDrawing,
      flatGuardDrawing
      -- TODO Add a nested test where the function expression is nested.
      ]
