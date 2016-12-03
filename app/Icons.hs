{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, RankNTypes, PartialTypeSignatures, ScopedTypeVariables #-}
module Icons
    (
    Icon(..),
    TransformableDia,
    getPortAngles,
    applyADia,
    flatLambda,
    iconToDiagram,
    textBox,
    multilineComment,
    enclosure,
    lambdaRegion,
    resultIcon,
    guardIcon,
    caseIcon,
    defaultLineWidth,
    ColorStyle(..),
    colorScheme,
    nestedApplyDia,
    coloredTextBox
    ) where

import Diagrams.Prelude hiding ((&), (#))

import Data.List(find)
import Data.Maybe(catMaybes, listToMaybe)

-- import Diagrams.Backend.SVG(B)
--import Diagrams.TwoD.Text(Text)
--import Data.Maybe(fromMaybe)

import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum)
import DrawingColors(colorScheme, ColorStyle(..))

-- TYPES --
type TransformableDia b n = Name -> Bool -> Angle n -> SpecialQDiagram b n

-- COLORS --
lineCol :: Colour Double
lineCol = lineC colorScheme

-- FUNCTIONS --
iconToDiagram :: SpecialBackend b n => Icon -> TransformableDia b n
iconToDiagram (ApplyAIcon n) = identDiaFunc $ applyADia n
iconToDiagram (PAppIcon n str) = pAppDia n str
iconToDiagram ResultIcon = identDiaFunc resultIcon
iconToDiagram BranchIcon = identDiaFunc branchIcon
iconToDiagram (TextBoxIcon s) = textBox s
iconToDiagram (BindTextBoxIcon s) = identDiaFunc $ bindTextBox s
iconToDiagram (GuardIcon n) = identDiaFunc $ guardIcon n
iconToDiagram (CaseIcon n) = identDiaFunc $ caseIcon n
iconToDiagram CaseResultIcon = identDiaFunc caseResult
iconToDiagram (FlatLambdaIcon n) = identDiaFunc $ flatLambda n
iconToDiagram (NestedApply args) = nestedApplyDia args
iconToDiagram (NestedPApp args) = nestedPAppDia args

applyPortAngles :: (Integral a, Floating n) => a -> [Angle n]
applyPortAngles x = fmap (@@ turn) $ case x of
  0 -> [3/8, 5/8] -- TODO Add back an angle of 1/2 for non-nested icons
  1 -> [0, 1/8, 7/8]
  _ -> [1/4, 3/4]

guardPortAngles :: (Integral a, Floating n) => a -> [Angle n]
guardPortAngles port = case port of
  0 -> [1/4 @@ turn]
  1 -> [3/4 @@ turn]
  _ -> otherAngles where otherAngles
                           | even port = [0 @@ turn]
                           | otherwise = [1/2 @@ turn]

findNestedIcon :: Name -> Icon -> Maybe Icon
findNestedIcon name icon = case icon of
  NestedApply args -> findIcon name args
  NestedPApp args -> findIcon name args
  _ -> Nothing

findIcon :: Name -> [Maybe (Name, Icon)] -> Maybe Icon
findIcon name args = icon where
  filteredArgs = catMaybes args
  nameMatches (n, _) = n == name
  icon = case filteredArgs of
    [] -> Nothing
    _ ->  case find nameMatches filteredArgs of
      Nothing -> listToMaybe $ catMaybes $ fmap (findNestedIcon name . snd) filteredArgs
      Just (_, finalIcon) -> Just finalIcon
  
nestedApplyPortAngles :: (Integral a, Floating n) => [Maybe (Name, Icon)] -> a -> Maybe Name -> [Angle n]
nestedApplyPortAngles args port maybeName = case maybeName of
  Nothing -> applyPortAngles port
  Just name -> case findIcon name args of
    Nothing -> []
    Just icon -> getPortAngles icon port Nothing

getPortAngles :: (Integral a, Floating n) => Icon -> a -> Maybe Name -> [Angle n]
getPortAngles icon port maybeName = case icon of
  ApplyAIcon _ -> applyPortAngles port
  PAppIcon _ _ -> applyPortAngles port
  ResultIcon -> []
  BranchIcon -> []
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  GuardIcon _ -> guardPortAngles port
  CaseIcon _ -> guardPortAngles port
  CaseResultIcon -> []
  FlatLambdaIcon _ -> applyPortAngles port
  NestedApply args -> nestedApplyPortAngles args port maybeName
  NestedPApp args -> nestedApplyPortAngles args port maybeName

-- END FUNCTIONS --

-- Make an identity TransformableDia

-- Warning: the first argument to nameDiagram can be almost any type,
-- so be careful with the parameter order.
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia name _ _ = nameDiagram name dia

-- | Names the diagram and puts all sub-names in the namespace of the top level name.
nameDiagram :: (IsName nm, SpecialNum n) => nm -> SpecialQDiagram b n -> SpecialQDiagram b n
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports (not === or |||)
--- since mempty has no size and will not be placed where you want it.
makePort :: SpecialNum n => Int -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

-- CONSTANTS --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

-- APPLY0 ICON --
circleRadius :: (Fractional a) => a
circleRadius = 0.5

apply0Triangle :: SpecialBackend b n => SpecialQDiagram b n
apply0Triangle = lw none $ rotateBy (-1/12) $ eqTriangle (2 * circleRadius)

portCircle :: SpecialBackend b n => SpecialQDiagram b n
portCircle = lw none $ fc lineCol $ circle (circleRadius * 0.5)

-- applyA Icon--
-- | apply0N port locations:
-- Port 0: Function
-- Port 1: Result
-- Ports 2,3..: Arguments
coloredApplyADia ::
  (SpecialBackend b n) =>
  Colour Double -> Int -> SpecialQDiagram b n
coloredApplyADia appColor n = centerXY finalDia where
  trianglePortsCircle = hcat [
    reflectX (fc appColor apply0Triangle),
    hcat $ take n $ map (\x -> makePort x <> portCircle <> strutX (circleRadius * 1.5)) [2,3..],
    makePort 1 <> alignR (lc appColor $ lwG defaultLineWidth $ fc appColor $ circle circleRadius)
    ]
  allPorts = makePort 0 <> alignL trianglePortsCircle
  topAndBottomLineWidth = width allPorts - circleRadius
  topAndBottomLine = alignL $ lwG defaultLineWidth $ lc appColor $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine === allPorts === topAndBottomLine

applyADia :: SpecialBackend b n => Int -> SpecialQDiagram b n
applyADia = coloredApplyADia (apply0C colorScheme)

--textApplyADia :: SpecialBackend b n =>
--  Int -> String -> TransformableDia b n
--textApplyADia = generalTextAppDia (textBoxTextC colorScheme) (apply0C colorScheme)

pAppDia :: SpecialBackend b n =>
  Int -> String -> TransformableDia b n
pAppDia = generalTextAppDia (patternTextC colorScheme) (patternC colorScheme)

--Get the decimal part of a float
reduceAngleRange :: SpecialNum a => a -> a
reduceAngleRange x = x - fromInteger (floor x)

generalTextAppDia :: SpecialBackend b n =>
  Colour Double -> Colour Double -> Int -> String -> TransformableDia b n
generalTextAppDia textCol borderCol numArgs str name reflect angle = nameDiagram name rotateDia where
  rotateDia = transformCorrectedTextBox str textCol borderCol reflect angle |||
    coloredApplyADia borderCol numArgs

transformCorrectedTextBox :: SpecialBackend b n =>
  String -> Colour Double -> Colour Double -> Bool -> Angle n -> SpecialQDiagram b n
transformCorrectedTextBox str textCol borderCol reflect angle =
  rotateBy textBoxRotation (reflectIfTrue reflect (coloredTextBox textCol (opaque borderCol) str))
  where
    reducedAngle = reduceAngleRange (angle ^. turn)
    textBoxRotation = if (reducedAngle > (1/4)) && (reducedAngle < (3/4)) then 1 / 2 else 0
    reflectIfTrue shouldReflect dia = if shouldReflect then reflectX dia else dia

nestedApplyDia :: SpecialBackend b n =>
  [Maybe (Name, Icon)] -> TransformableDia b n
nestedApplyDia = generalNestedDia (apply0C colorScheme)

nestedPAppDia :: SpecialBackend b n =>
  [Maybe (Name, Icon)] -> TransformableDia b n
nestedPAppDia = generalNestedDia (patternC colorScheme)

generalNestedDia :: SpecialBackend b n =>
  Colour Double -> [Maybe (Name, Icon)] -> TransformableDia b n
generalNestedDia borderCol funcNameAndArgs name reflect angle = named name $ case funcNameAndArgs of
  [] -> mempty
  (maybeFunText:args) -> centerXY $  transformedText ||| centerY finalDia
    where
      makeQualifiedPort x = name .>> makePort x
      transformedText = case maybeFunText of
        Just _ -> makeInnerIcon 0 maybeFunText
        Nothing -> mempty
      seperation = circleRadius * 1.5
      verticalSeperation = circleRadius
      trianglePortsCircle = hsep seperation $
        reflectX (fc borderCol apply0Triangle) :
        zipWith makeInnerIcon [2,3..] args ++
        [makeQualifiedPort 1 <> alignR (lc borderCol $ lwG defaultLineWidth $ fc borderCol $ circle circleRadius)]
  
      allPorts = makeQualifiedPort 0 <> alignL trianglePortsCircle
      topAndBottomLineWidth = width allPorts - circleRadius
      argBox = alignL $ lwG defaultLineWidth $ lc borderCol $ rect topAndBottomLineWidth (height allPorts + verticalSeperation)
      finalDia = argBox <> allPorts

      makeInnerIcon portNum Nothing = makeQualifiedPort portNum <> portCircle
      makeInnerIcon _ (Just (iconName, icon)) = iconToDiagram icon iconName reflect angle


-- TEXT ICON --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 1
monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.61
textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.1

textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t name reflect angle = nameDiagram name $ transformCorrectedTextBox t (textBoxTextC colorScheme) (textBoxC colorScheme) reflect angle

bindTextBox :: SpecialBackend b n =>
  String -> SpecialQDiagram b n
bindTextBox = coloredTextBox (bindTextBoxTextC colorScheme) $ opaque (bindTextBoxC colorScheme)

multilineComment :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment textColor boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (commentTextArea textColor) textLines
    textDia = vcat textAreas

-- | Given the number of letters in a textbox string, make a rectangle that will
-- enclose the text box.
rectForText :: (InSpace V2 n t, TrailLike t) => Int -> t
rectForText n = rect rectangleWidth (textBoxFontSize * textBoxHeightFactor)
  where
    rectangleWidth = fromIntegral n * textBoxFontSize * monoLetterWidthToHeightFraction
      + (textBoxFontSize * 0.2)

-- Since the normal SVG text has no size, some hackery is needed to determine
-- the size of the text's bounding box.
coloredTextBox :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor boxColor t =
  fontSize (local textBoxFontSize) (bold $ font "freemono" $ fc textColor $ text t)
  <>  lwG (0.6 * defaultLineWidth) (lcA boxColor $ rectForText (length t))

commentTextArea :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
commentTextArea textColor t =
  alignL $ fontSize (local textBoxFontSize) (font "freemono" $ fc textColor $ topLeftText t)
  <>  alignTL (lw none $ rectForText (length t))

-- ENCLOSING REGION --
enclosure :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
enclosure dia = dia <> lwG defaultLineWidth (lc (regionPerimC colorScheme) $ boundingRect (frame 0.5 dia))

-- LAMBDA ICON --
-- Don't use === here to put the port under the text box since mempty will stay
-- at the origin of the text box.
lambdaIcon ::
  SpecialBackend b n =>
  Int -> SpecialQDiagram b n
lambdaIcon x = alignB (coloredTextBox (lamArgResC colorScheme) transparent "λ") <> makePort x

-- LAMBDA REGION --

-- | lambdaRegion takes as an argument the numbers of parameters to the lambda,
-- and draws the diagram inside a region with the lambda icons on top.
lambdaRegion :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n -> SpecialQDiagram b n
lambdaRegion n dia =
  centerXY $ centerX lambdaIcons === centerX (enclosure dia)
  where lambdaIcons = hsep 0.4 (take n (map lambdaIcon [0,1..]))

-- RESULT ICON --
resultIcon :: SpecialBackend b n => SpecialQDiagram b n
resultIcon =  lw none $ fc (lamArgResC colorScheme) unitSquare

-- BRANCH ICON --
branchIcon :: SpecialBackend b n => SpecialQDiagram b n
branchIcon = lw none $ lc lineCol $ fc lineCol $ circle circleRadius

-- GUARD ICON --
guardSize :: (Fractional a) => a
guardSize = 0.7

guardTriangle :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n
guardTriangle x =
  alignL $ alignR (triangleAndPort ||| lwG defaultLineWidth (hrule (guardSize * 0.8))) <> makePort x
  where
    triangleAndPort = alignR $ alignT $ lwG defaultLineWidth $ rotateBy (1/8) $
      polygon (polyType .~ PolySides [90 @@ deg, 45 @@ deg] [guardSize, guardSize] $ with)

guardLBracket :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n
guardLBracket x = alignL (alignT ell) <> makePort x
  where
    ellShape = fromOffsets $ map r2 [(0, guardSize), (-guardSize,0)]
    ell = lineJoin LineJoinRound $ lwG defaultLineWidth $ lc (boolC colorScheme) (strokeLine ellShape)

-- | generalGuardIcon port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalGuardIcon :: SpecialBackend b n =>
  Colour Double -> (Int -> SpecialQDiagram b n) -> SpecialQDiagram b n -> Int -> SpecialQDiagram b n
generalGuardIcon triangleColor lBracket bottomDia n = centerXY $ alignT (bottomDia <> makePort 1) <> alignB (bigVerticalLine <> guardDia <> makePort 0)
  where
    --guardTriangles = vsep 0.4 (take n (map guardTriangle [0,1..]))
    trianglesWithPorts = map guardTriangle [2,4..]
    lBrackets = map lBracket [3, 5..]
    trianglesAndBrackets =
      zipWith zipper trianglesWithPorts lBrackets
    zipper thisTriangle lBrack = verticalLine === (alignR (extrudeRight guardSize lBrack) <> lc triangleColor (alignL thisTriangle))
      where
        verticalLine = strutY 0.4
    guardDia = vcat (alignT $ take n trianglesAndBrackets)
    bigVerticalLine = alignT $ lwG defaultLineWidth $ lc triangleColor $ vrule (height guardDia)

-- | The ports of the guard icon are as follows:
-- Port 0: Top result port
-- Port 1: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
guardIcon :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n
guardIcon = generalGuardIcon lineCol guardLBracket mempty

-- TODO Improve design to be more than a circle.
caseResult :: SpecialBackend b n =>
  SpecialQDiagram b n
caseResult = lw none $ lc caseCColor $ fc caseCColor $ circle (circleRadius * 0.7)
  where
    caseCColor = caseRhsC colorScheme

caseC :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n
caseC n = caseResult <> makePort n


-- | The ports of the case icon are as follows:
-- Port 0: Top result port
-- Port 1: Bottom result port
-- Ports 3,5...: The left ports for the results
-- Ports 2,4...: The right ports for the patterns
caseIcon :: SpecialBackend b n =>
  Int -> SpecialQDiagram b n
caseIcon = generalGuardIcon (patternC colorScheme) caseC caseResult

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
flatLambda :: SpecialBackend b n => Int -> SpecialQDiagram b n
flatLambda n = finalDia where
  lambdaCircle = lwG defaultLineWidth $ lc (regionPerimC colorScheme) $ fc (regionPerimC colorScheme) $ circle circleRadius
  lambdaParts = (makePort 0 <> resultIcon) : (portIcons ++  [makePort 1 <> alignR lambdaCircle])
  portIcons = take n $ map (\x -> makePort x <> portCircle) [2,3..]
  middle = alignL (hsep 0.5 lambdaParts)
  topAndBottomLineWidth = width middle - circleRadius
  topAndBottomLine = alignL $ lwG defaultLineWidth $ lc (regionPerimC colorScheme) $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine <> alignB (topAndBottomLine <> alignT middle)
