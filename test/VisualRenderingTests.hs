{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module VisualRenderingTests (
  renderTests
  ) where
import qualified Diagrams.Prelude as Dia

import Rendering (renderDrawing)
import Types (Labeled(..), NodeName(..), Drawing(..), Edge, Icon(..), Port(..)
             , SpecialQDiagram, SpecialBackend
             , Named(..), NamedIcon)

import Util(iconToPort, tupleToNamed)

-- TODO Fix these tests such that they test nested icons correctly. Will need to
-- change the Drawing type.

iconToIntPort :: NodeName -> NodeName -> Int -> Edge
iconToIntPort x y p = iconToPort x y (Port p)

n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10 :: NodeName
nodeNames :: [NodeName]
nodeNames@[n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10]
  = fmap NodeName [0..10]

ni0, ni1, ni2, ni3, ni4, ni5, ni6, ni7, ni8, ni9, ni10 :: Icon -> NamedIcon
[ni0, ni1, ni2, ni3, ni4, ni5, ni6, ni7, ni8, ni9, ni10]
  = fmap Named nodeNames

textBoxIcons :: [NamedIcon]
textBoxIcons =
  [ ni2 (TextBoxIcon "n2")
  , ni4 (TextBoxIcon "n4")
  , ni10 $ TextBoxIcon "n10"
  , ni6 (TextBoxIcon "n6")
  , ni7 (TextBoxIcon "n7")
  , ni8 (TextBoxIcon "n8")
  , ni9 (TextBoxIcon "n9")]

-- TODO refactor these Drawings
nestedCaseDrawing :: Drawing
nestedCaseDrawing = Drawing icons [] where
  icons = textBoxIcons <> fmap tupleToNamed [
    (n0, NestedCaseIcon [Nothing, Nothing, Nothing]),
    (n1, NestedCaseIcon [Nothing, Just n2, Nothing]),
    (n3, NestedCaseIcon [Nothing, Nothing, Just n4]),
    (n5, NestedCaseIcon [Nothing,
                         Just n6,
                         Just n7,
                         Just n8,
                         Just n9])
    ]

nestedMultiIfDrawing :: Drawing
nestedMultiIfDrawing = Drawing icons edges where
  icons = textBoxIcons <>
    [ ni0 $ NestedMultiIfIcon [Nothing, Nothing, Nothing]
    , ni1 $ NestedMultiIfIcon [Nothing, Just n2, Nothing]
    , ni3 $ NestedMultiIfIcon [Nothing, Nothing, Just n4]
    , ni5 $ NestedMultiIfIcon [Nothing,
                         Just n6,
                         Just n7,
                         Just n8,
                         Just n9]
    ]
  edges = [
    iconToIntPort n10 n5 0
    ]

flatCaseDrawing :: Drawing
flatCaseDrawing = Drawing icons edges where
  icons = fmap tupleToNamed [
    (NodeName 0, CaseIcon 0),
    (NodeName 1, CaseIcon 1),
    (NodeName 2, CaseIcon 2)
    ]
  edges = []

flatMultiIfDrawing :: Drawing
flatMultiIfDrawing = Drawing icons edges where
  icons = fmap tupleToNamed [
    (NodeName 1, MultiIfIcon 1),
    (NodeName 2, MultiIfIcon 2),
    (NodeName 3, MultiIfIcon 3)
    ]
  edges = []

nestedPAppDia :: Drawing
nestedPAppDia = Drawing icons []
  where
    icons = [
      Named n0 (NestedPApp (Labeled Nothing "baz") [])
      , Named
        (NodeName 2)
        (NestedPApp
          (Labeled Nothing "")
          [ Labeled (Just (Named (NodeName 1) (TextBoxIcon "foo"))) "bar"
          , Labeled Nothing "bar"])
      , Named
        (NodeName 3)
        (NestedPApp
          (Labeled (Just (Named (NodeName 4) (TextBoxIcon "foo"))) "bar")
          [Labeled Nothing "bar"])
      ]

-- TODO Uncomment
-- nestedApplyDia :: Drawing
-- nestedApplyDia = Drawing icons []
--   where
--     icons = [
--       Named
--       (NodeName 1)
--       (NestedApply
--         ApplyNodeFlavor
--         -- TODO Uncomment
--         -- (Just $ Named (NodeName 1) (TextBoxIcon "foo"))
--         (Just $ NodeName 2)
--         [])
--       ]

lambdaDia :: Drawing
lambdaDia = Drawing icons []
  where
    icons = [
      ni0 $ LambdaIcon ["foo", "bar"] Nothing [n0, n1]
      , ni1 CaseResultIcon
      , ni2 $ MultiIfIcon 3
      ]

-- TODO Uncomment
-- nestedLambdaDia :: Drawing
-- nestedLambdaDia = Drawing icons []
--   where
--     icons = [
--       ni0 $ LambdaIcon
--         ["baz", "cat"]
--         (Just $ Named n2 (TextBoxIcon "foobar"))
--         [n0, n1]
--       , ni1 CaseResultIcon
--       , ni2 $ MultiIfIcon 3
--       ]


--renderTests :: IO (Diagram B)
renderTests :: SpecialBackend b Double => IO (SpecialQDiagram b Double)
renderTests = do
  renderedDiagrams <- traverse (renderDrawing "") allDrawings
  let vCattedDrawings = Dia.vsep 0.5 renderedDiagrams
  pure vCattedDrawings
  where
    -- TODO Re-enable tests
    allDrawings = [
      nestedCaseDrawing
      , nestedMultiIfDrawing
      , flatCaseDrawing
      , flatMultiIfDrawing
      , nestedPAppDia
      -- , nestedApplyDia
      , lambdaDia
      -- , nestedLambdaDia
      ]
