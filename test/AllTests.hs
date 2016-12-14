import Prelude hiding (return)

import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Backend.SVG.CmdLine(B)
import Diagrams.Prelude hiding ((#), (&))

import Test.HUnit

import Icons(colorScheme, ColorStyle(..))

import UnitTests(allUnitTests)
import VisualGraphAlgorithmTests(visualCollapseTests)
import VisualRenderingTests(renderTests)
import VisualTranslateTests(visualTranslateTests)


drawingsAndNames :: [(String, IO (Diagram B))]
drawingsAndNames = [
  ("translate-tests", visualTranslateTests),
  ("render-tests", renderTests),
  ("collapse-tests", visualCollapseTests)
  ]

renderDrawings :: [(String, IO (Diagram B))] -> IO ()
renderDrawings = mapM_ saveDrawing where
  saveDrawing (name, drawingMaker) = do
    dia <- drawingMaker
    -- TODO Replace string concatenation with proper path manipulation functions.
    renderSVG ("test/test-output/" ++ name ++ ".svg") (mkWidth 700) (bgFrame 1 (backgroundC colorScheme) dia)

main :: IO ()
--main = print "Hello world"
main = do
  --  ING.prettyPrint singleApplyGraph
  renderDrawings drawingsAndNames
  _ <- runTestTT allUnitTests
  pure ()
