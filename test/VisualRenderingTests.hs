{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualRenderingTests (
  renderTests
  ) where

import Diagrams.Prelude hiding ((#), (&))

import Rendering (renderDrawing)
import Types (Labeled(..), NodeName(..), Drawing(..), Edge, Icon(..), Port(..), LikeApplyFlavor(..), SpecialQDiagram, SpecialBackend, NamedIcon(..))

import Util(iconToPort, tupleToNamedIcon)


iconToIntPort :: NodeName -> NodeName -> Int -> Edge
iconToIntPort x y p = iconToPort x y (Port p)

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

nestedPAppDia :: Drawing
nestedPAppDia = Drawing icons []
  where
    icons = [
      NamedIcon (NodeName 1) (NestedPApp (Labeled Nothing "baz") [])
      , NamedIcon
        (NodeName 2)
        (NestedPApp
          (Labeled Nothing "")
          [ Labeled (Just (NamedIcon (NodeName 1) (TextBoxIcon "foo"))) "bar"
          , Labeled Nothing "bar"])
      , NamedIcon
        (NodeName 3)
        (NestedPApp
          (Labeled (Just (NamedIcon (NodeName 4) (TextBoxIcon "foo"))) "bar")
          [Labeled Nothing "bar"])
      ]

nestedApplyDia :: Drawing
nestedApplyDia = Drawing icons []
  where
    icons = [
      NamedIcon
      (NodeName 1)
      (NestedApply
        ApplyNodeFlavor
        (Just $ NamedIcon (NodeName 1) (TextBoxIcon "foo"))
        [])
        --[Just $ NamedIcon (NodeName 1) (TextBoxIcon "bar")])
      ]

--renderTests :: IO (Diagram B)
renderTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
renderTests = do
  renderedDiagrams <- traverse renderDrawing allDrawings
  let vCattedDrawings = vsep 0.5 renderedDiagrams
  pure vCattedDrawings
  where
    allDrawings = [
      nestedCaseDrawing,
      nestedGuardDrawing,
      flatCaseDrawing,
      flatGuardDrawing,
      nestedPAppDia,
      nestedApplyDia
      ]
