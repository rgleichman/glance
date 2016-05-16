{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where
import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would require
-- - an special case when translating when Glance is run on its own source code.
import Diagrams.Prelude hiding ((#), (&))
import Diagrams.Backend.SVG.CmdLine
import qualified Language.Haskell.Exts as Exts

import Icons(ColorStyle(..), colorScheme)
import Rendering(renderDrawing)
import Translate(drawingsFromModule)


-- TODO Now --
-- Fix icon nesting if a non-nestable icon (eg. flatLambdaIcon) is part of the expression.
-- - eg. y = f $ g (\x -> x)
-- Fix rotation missing edges to nested diagrams.

-- Add a maximum nesting depth.
-- Clean up Rendering and Icons.

-- Refactor Translate
-- Add documentation.
-- Add comments as text boxes, and use that to make a getting started guide for the README.

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
-- Investigate arrows not being drawn

-- Translate todos:
-- Make nested version of FlatLambdaIcon
-- Fix test case x of {0 -> 1; y -> y}.
-- Add proper RecConstr, and RecUpdate support.
-- Eliminate BranchIcon in Alts.
-- Eliminate BranchIcon for the identity funciton "y x = x"
-- otherwise Guard special case

renderFile :: String -> IO (Diagram B)
renderFile inputFilename= do
  parseResult <- Exts.parseFileWithExts [Exts.EnableExtension Exts.MultiParamTypeClasses, Exts.EnableExtension Exts.FlexibleContexts]
    inputFilename
  let
    parsedModule = Exts.fromParseResult parseResult
    drawings = drawingsFromModule parsedModule
  print parsedModule
  print "\n\n"
  --print drawings

  diagrams <- traverse renderDrawing drawings
  let
    vCattedDrawings = vsep 1 $ fmap alignL diagrams
  pure (bgFrame 1 (backgroundC colorScheme) vCattedDrawings :: Diagram B)


main :: IO ()
main = mainWith renderFile
