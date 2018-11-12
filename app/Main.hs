{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main
  (main
  , CmdLineOptions(..)) where

import Prelude hiding (return)

-- Note: (#) and (&) are hidden in all Glance source files, since they would
-- require a special case when translating when Glance is run on its own source
-- code.
import qualified Diagrams.Prelude as Dia hiding ((#), (&))

import qualified Language.Haskell.Exts as Exts

-- Options.Applicative does not seem to work qualified
import Options.Applicative(header, progDesc, fullDesc, helper, info
                          , defaultPrefs, customExecParser, help, short, switch
                          , metavar, auto, argument, str, prefShowHelpOnError
                          , Parser)

import Icons(ColorStyle(..), colorScheme, multilineComment)
import Rendering(renderIngSyntaxGraph)
import Translate(translateModuleToCollapsedGraphs)
import Util(customRenderSVG)

data CmdLineOptions = CmdLineOptions {
  cmdInputFilename :: String,
  cmdOutputFilename :: String,
  cmdImageWidth :: Double,
  cmdIncludeComments :: Bool
  }

optionParser :: Parser CmdLineOptions
optionParser = CmdLineOptions
  <$> argument str (metavar "INPUT_FILE" Dia.<> help "Input .hs filename")
  <*> argument str (metavar "OUTPUT_FILE" Dia.<> help "Output .svg filename")
  <*> argument auto (metavar "IMAGE_WIDTH" Dia.<> help "Output image width")
  <*> switch
  (short 'c' Dia.<> help "Include comments between top level declarations.")

renderFile :: CmdLineOptions -> IO ()
renderFile (CmdLineOptions
             inputFilename
             outputFilename
             imageWidth
             includeComments)
  = do
  putStrLn $ "Translating file " ++ inputFilename ++ " into a Glance image."
  parseResult <- Exts.parseFileWithComments
    (Exts.defaultParseMode {
        Exts.extensions = [Exts.EnableExtension Exts.MultiParamTypeClasses
                          , Exts.EnableExtension Exts.FlexibleContexts]
        , Exts.parseFilename = inputFilename
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
    commentsInBoxes
      = fmap
        (\(Exts.Comment _ _ c) ->
            Dia.alignL $ multilineComment Dia.white (Dia.opaque Dia.white) c)
        comments
    diagramsAndComments
      = Dia.vsep 2 $ zipWith
        (\x y -> x Dia.=== Dia.strutY 0.4 Dia.=== y)
        commentsInBoxes
        (fmap Dia.alignL diagrams)
    justDiagrams = Dia.vsep 1 $ fmap Dia.alignL diagrams
    diagramsAndMaybeComments
      = if includeComments then diagramsAndComments else justDiagrams
  --print comments

    finalDia = Dia.bgFrame 1 (backgroundC colorScheme) diagramsAndMaybeComments
  customRenderSVG outputFilename (Dia.mkWidth imageWidth) finalDia
  putStrLn $ "Successfully wrote " ++ outputFilename

translateFileMain :: IO ()
translateFileMain = customExecParser parserPrefs  opts >>= renderFile where

  parserPrefs = defaultPrefs{
    prefShowHelpOnError = True
    }

  opts = info (helper <*> optionParser)
    (fullDesc
    Dia.<> progDesc "Translate a Haskell source file (.hs) into an SVG image."
    Dia.<> header "Glance - a visual representation of Haskell")

main :: IO ()
main = translateFileMain
