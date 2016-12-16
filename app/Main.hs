{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where
import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would require
-- - an special case when translating when Glance is run on its own source code.
import Diagrams.Prelude hiding ((#), (&))
import Diagrams.Backend.SVG.CmdLine
--import Diagrams.Backend.Rasterific.CmdLine
import qualified Language.Haskell.Exts as Exts

import Icons(ColorStyle(..), colorScheme, multilineComment)
import Rendering(renderIngSyntaxGraph)
import Translate(translateModuleToCollapsedGraphs)


renderFile :: String -> String -> IO (Diagram B)
renderFile inputFilename includeComments = do
  putStrLn $ "Translating file " ++ inputFilename ++ " into a Glance image."
  parseResult <- Exts.parseFileWithComments
    (Exts.defaultParseMode
      {Exts.extensions = [Exts.EnableExtension Exts.MultiParamTypeClasses, Exts.EnableExtension Exts.FlexibleContexts],
      Exts.parseFilename = inputFilename
      })
    inputFilename
  let
    (parsedModule, comments) = Exts.fromParseResult parseResult
    drawings = translateModuleToCollapsedGraphs parsedModule
  --print parsedModule
  --print "\n\n"
  --print drawings

  diagrams <- traverse renderIngSyntaxGraph drawings
  let
    commentsInBoxes = fmap (\(Exts.Comment _ _ c) -> alignL $ multilineComment white (opaque white) c) comments
    diagramsAndComments = vsep 2 $ zipWith (\x y -> x === strutY 0.4 === y) commentsInBoxes (fmap alignL diagrams)
    justDiagrams = vsep 1 $ fmap alignL diagrams
    diagramsAndMaybeComments = if includeComments == "c" then diagramsAndComments else justDiagrams
  --print comments

  pure (bgFrame 1 (backgroundC colorScheme) diagramsAndMaybeComments :: Diagram B)


main :: IO ()
main = do
  mainWith renderFile
  putStrLn "Successfully translated file."
