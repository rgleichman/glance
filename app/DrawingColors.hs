module DrawingColors (
  ColorStyle(..),
  colorScheme
) where

import Diagrams.Prelude hiding ((&), (#))

-- COLO(U)RS --
colorScheme :: ColorStyle Double
colorScheme = colorOnBlackScheme

data ColorStyle a = ColorStyle {
  backgroundC :: Colour a,
  lineC :: Colour a,
  textBoxTextC :: Colour a,
  textBoxC :: Colour a,
  apply0C :: Colour a,
  apply1C :: Colour a,
  boolC :: Colour a,
  lamArgResC :: Colour a,
  regionPerimC :: Colour a,
  caseRhsC :: Colour a,
  patternC :: Colour a,
  patternTextC :: Colour a,
  bindTextBoxC :: Colour a,
  bindTextBoxTextC :: Colour a
}

colorOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
colorOnBlackScheme = ColorStyle {
  backgroundC = black,
  lineC = white,
  --lineC = lightgray,
  textBoxTextC = white,
  textBoxC = white,
  apply0C = red,
  apply1C = cyan,
  boolC = orange,
  lamArgResC = lightSlightlyPurpleBlue,
  regionPerimC = lime,
  caseRhsC = slightlyGreenYellow,
  patternC = lightMagenta,
  patternTextC = cyan,
  bindTextBoxC = reddishOrange,
  bindTextBoxTextC = lime
}
  where
    slightlyGreenYellow = sRGB24 212 255 0
    lightMagenta = sRGB24 255 94 255
    lightSlightlyPurpleBlue = sRGB24 67 38 255
    reddishOrange = sRGB24 255 119 0

whiteOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
whiteOnBlackScheme = ColorStyle {
  backgroundC = black,
  lineC = white,
  textBoxTextC = white,
  textBoxC = white,
  apply0C = white,
  apply1C = white,
  boolC = white,
  lamArgResC = white,
  regionPerimC = white,
  caseRhsC = white,
  patternC = white,
  patternTextC = white,
  bindTextBoxC = white,
  bindTextBoxTextC = white
}

-- Use this to test that all of the colors use the colorScheme
randomColorScheme :: (Floating a, Ord a) => ColorStyle a
randomColorScheme = ColorStyle {
  backgroundC = darkorchid,
  lineC = yellow,
  textBoxTextC = blue,
  textBoxC = magenta,
  apply0C = orange,
  apply1C = green,
  boolC = lightpink,
  lamArgResC = red,
  regionPerimC = cyan,
  caseRhsC = red,
  patternC = olive,
  patternTextC = coral,
  bindTextBoxC = maroon,
  bindTextBoxTextC = lime
}
