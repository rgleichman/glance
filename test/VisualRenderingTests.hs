{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualRenderingTests (
  renderTests
  ) where
import qualified Diagrams.Prelude as Dia

import Rendering (renderDrawing)
import Types (Labeled(..), NodeName(..), Drawing(..), Edge, Icon(..), Port(..)
             , LikeApplyFlavor(..), SpecialQDiagram, SpecialBackend
             , NamedIcon(..))

import Util(iconToPort, tupleToNamedIcon)

iconToIntPort :: NodeName -> NodeName -> Int -> Edge
iconToIntPort x y p = iconToPort x y (Port p)

n0, n1, _n2, n3, _n4, n5, _n6, _n7, _n8, _n9, n10 :: NodeName
nodeNames :: [NodeName]
nodeNames@[n0, n1, _n2, n3, _n4, n5, _n6, _n7, _n8, _n9, n10]
  = fmap NodeName [0..10]

ni0, ni1, ni2, ni3, ni4, ni5, ni6, ni7, ni8, ni9, ni10 :: Icon -> NamedIcon
[ni0, ni1, ni2, ni3, ni4, ni5, ni6, ni7, ni8, ni9, ni10]
  = fmap NamedIcon nodeNames

-- TODO refactor these Drawings
nestedCaseDrawing :: Drawing
nestedCaseDrawing = Drawing icons [] where
  icons = fmap tupleToNamedIcon [
    (n0, NestedCaseIcon [Nothing, Nothing, Nothing]),
    (n1, NestedCaseIcon [Nothing, Just $ ni2 (TextBoxIcon "n2"), Nothing]),
    (n3, NestedCaseIcon [Nothing, Nothing, Just $ ni4 (TextBoxIcon "n4")]),
    (n5, NestedCaseIcon [Nothing,
                         Just $ ni6 (TextBoxIcon "n6"),
                         Just $ ni7 (TextBoxIcon "n7"),
                         Just $ ni8 (TextBoxIcon "n8"),
                         Just $ ni9 (TextBoxIcon "n9")])
    ]

nestedGuardDrawing :: Drawing
nestedGuardDrawing = Drawing icons edges where
  icons = [
    ni10 $ TextBoxIcon "n10"
    , ni0 $ NestedGuardIcon [Nothing, Nothing, Nothing]
    , ni1 $ NestedGuardIcon [Nothing, Just $ ni2 (TextBoxIcon "n2"), Nothing]
    , ni3 $ NestedGuardIcon [Nothing, Nothing, Just $ ni4 (TextBoxIcon "n4")]
    , ni5 $ NestedGuardIcon [Nothing,
                         Just $ ni6 (TextBoxIcon "n6"),
                         Just $ ni7 (TextBoxIcon "n7"),
                         Just $ ni8 (TextBoxIcon "n8"),
                         Just $ ni9 (TextBoxIcon "n9")]
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
      NamedIcon n0 (NestedPApp (Labeled Nothing "baz") [])
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
      ]

lambdaDia :: Drawing
lambdaDia = Drawing icons []
  where
    icons = [
      ni0 $ FlatLambdaIcon ["foo", "bar"] [n0, n1]
      , ni1 CaseResultIcon
      , ni2 $ GuardIcon 3
      ]


--renderTests :: IO (Diagram B)
renderTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
renderTests = do
  renderedDiagrams <- traverse renderDrawing allDrawings
  let vCattedDrawings = Dia.vsep 0.5 renderedDiagrams
  pure vCattedDrawings
  where
    allDrawings = [
      nestedCaseDrawing
      , nestedGuardDrawing
      , flatCaseDrawing
      , flatGuardDrawing
      , nestedPAppDia
      , nestedApplyDia
      , lambdaDia
      ]
