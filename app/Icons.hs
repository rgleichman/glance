{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Icons
    (
    Icon(..),
    TransformableDia,
    getPortAngles,
    iconToDiagram,
    resultPort,
    textBox,
    multilineComment,
    defaultLineWidth,
    ColorStyle(..),
    colorScheme,
    coloredTextBox
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)

import Data.List(find)
import Data.Maybe(catMaybes, listToMaybe, isJust, fromJust)
import Data.Either(partitionEithers)
import qualified Control.Arrow as Arrow

import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum, NodeName, Port(..), LikeApplyFlavor(..),
            SyntaxNode)
import DrawingColors(colorScheme, ColorStyle(..))

-- TYPES --
-- | A TransformableDia is a function that returns a diagram for an icon when given
-- the icon's name, its nesting depth, whether it will be reflected, and by what
-- angle it will be rotated.
type TransformableDia b n = NodeName -> Int -> Bool -> Angle n -> SpecialQDiagram b n

-- CONSTANTS --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

circleRadius :: (Fractional a) => a
circleRadius = 0.5

-- COLORS --
lineCol :: Colour Double
lineCol = lineC colorScheme


-- BEGIN Exported icon functions --

iconToDiagram :: SpecialBackend b n => Icon -> TransformableDia b n
iconToDiagram icon = case icon of
  ApplyAIcon n -> nestedApplyDia ApplyNodeFlavor $ replicate (1 + n) Nothing
  ComposeIcon n -> nestedApplyDia ComposeNodeFlavor $ replicate (1 + n) Nothing
  PAppIcon n str -> generalTextAppDia (patternTextC colorScheme) (patternC colorScheme) n str
  TextBoxIcon s -> textBox s
  BindTextBoxIcon s -> identDiaFunc $ bindTextBox s
  GuardIcon n -> nestedGuardDia $ replicate (1 + (2 * n)) Nothing
  CaseIcon n -> nestedCaseDia $ replicate (1 + (2 * n)) Nothing
  CaseResultIcon -> identDiaFunc caseResult
  FlatLambdaIcon n -> identDiaFunc $ flatLambda n
  NestedApply flavor args -> nestedApplyDia flavor args
  NestedPApp args -> nestedPAppDia (repeat $ patternC colorScheme) args
  NestedCaseIcon args -> nestedCaseDia args
  NestedGuardIcon args -> nestedGuardDia args

-- BEGIN getPortAngles --

applyPortAngles :: Floating n => Port -> [Angle n]
applyPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [3/8, 1/2, 5/8] -- TODO Don't use angle of 1/2 for nested icons here
  --1 -> [1/8, 7/8, 0]
  1 -> [0]
  _ -> [1/4, 3/4]

pAppPortAngles :: Floating n => Port -> [Angle n]
pAppPortAngles (Port x) = fmap (@@ turn) $ case x of
  0 -> [1/4]
  1 -> [0]
  _ -> [1/2]

guardPortAngles :: Floating n => Port -> [Angle n]
guardPortAngles (Port port) = case port of
  0 -> [1/4 @@ turn]
  1 -> [3/4 @@ turn]
  _ -> otherAngles where otherAngles
                           | even port = [0 @@ turn]
                           | otherwise = [1/2 @@ turn]

findNestedIcon :: NodeName -> Icon -> Maybe Icon
findNestedIcon name icon = case icon of
  NestedApply _ args -> snd <$> findIcon name args
  NestedPApp args -> snd <$> findIcon name args
  _ -> Nothing

findIcon :: NodeName -> [Maybe (NodeName, Icon)] -> Maybe (Int, Icon)
findIcon name args = icon where
  numberedArgs = zip ([0,1..] :: [Int]) args
  filteredArgs = Arrow.second fromJust <$> filter (isJust . snd) numberedArgs
  nameMatches (_, (n, _)) = n == name
  icon = case find nameMatches filteredArgs of
    Nothing -> listToMaybe $ catMaybes $ fmap findSubSubIcon filteredArgs
    Just (argNum, (_, finalIcon)) -> Just (argNum, finalIcon)
    where
      findSubSubIcon (argNum, (_, subIcon)) = case findNestedIcon name subIcon of
        Nothing -> Nothing
        Just x -> Just (argNum, x)

generalNestedPortAngles :: SpecialNum n =>
  (Port -> [Angle n]) -> [Maybe (NodeName, Icon)] -> Port -> Maybe NodeName -> [Angle n]
generalNestedPortAngles defaultAngles args port maybeNodeName = case maybeNodeName of
  Nothing -> defaultAngles port
  Just name -> case findIcon name args of
    Nothing -> []
    Just (_, icon) -> getPortAngles icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

-- TODO reflect the angles for the right side sub-icons
nestedGuardPortAngles :: SpecialNum n => [Maybe (NodeName, Icon)] -> Port -> Maybe NodeName -> [Angle n]
nestedGuardPortAngles args port maybeNodeName = case maybeNodeName of
  Nothing -> guardPortAngles port
  Just name -> case findIcon name args of
    Nothing -> []
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (argNum, icon) -> if odd argNum && argNum >= 1
      -- The icon will be reflected
      then fmap reflectXAngle subAngles
      else subAngles
      where
        subAngles = getPortAngles icon port Nothing

getPortAngles :: SpecialNum n => Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAngles icon port maybeNodeName = case icon of
  ApplyAIcon _ -> applyPortAngles port
  ComposeIcon _ -> applyPortAngles port
  PAppIcon _ _ -> applyPortAngles port
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  GuardIcon _ -> guardPortAngles port
  CaseIcon _ -> guardPortAngles port
  CaseResultIcon -> []
  FlatLambdaIcon _ -> applyPortAngles port
  NestedApply _ args -> generalNestedPortAngles applyPortAngles args port maybeNodeName
  NestedPApp args -> generalNestedPortAngles pAppPortAngles args port maybeNodeName
  NestedCaseIcon args -> nestedGuardPortAngles args port maybeNodeName
  NestedGuardIcon args -> nestedGuardPortAngles args port maybeNodeName

-- END getPortAngles --

-- BEGIN Port numbers

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
resultPort :: SyntaxNode -> Port
resultPort = const (Port 1)

-- END Port numbers

-- END Exported icon functions --


-- BEGIN Diagram helper functions --

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia name _ _ _ = nameDiagram name dia

-- | Names the diagram and puts all sub-names in the namespace of the top level name.
nameDiagram :: SpecialNum n => NodeName -> SpecialQDiagram b n -> SpecialQDiagram b n
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports (not === or |||)
--- since mempty has no size and will not be placed where you want it.
makePort :: SpecialNum n => Port -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialNum n => NodeName -> Port -> SpecialQDiagram b n
makeQualifiedPort n x = n .>> makePort x

-- END Diagram helper functions


-- BEGIN Icons --

-- BEGIN Sub-diagrams --

apply0Triangle :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
apply0Triangle col = fc col $ lw none $ rotateBy (-1/12) $ eqTriangle (2 * circleRadius)

composeSemiCircle :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
composeSemiCircle col = lc col $ lwG defaultLineWidth $ wedge circleRadius yDir halfTurn

portCircle :: SpecialBackend b n => SpecialQDiagram b n
portCircle = lw none $ fc lineCol $ circle (circleRadius * 0.5)

resultIcon :: SpecialBackend b n => SpecialQDiagram b n
resultIcon =  lw none $ fc (lamArgResC colorScheme) unitSquare

-- END Sub-diagrams

-- BEGIN Main icons

-- BEGIN Apply like icons

-- | apply port locations:
-- Port 0: Function
-- Port 1: Result
-- Ports 2,3..: Arguments
coloredApplyADia ::
  (SpecialBackend b n) =>
  Colour Double -> Int -> SpecialQDiagram b n
coloredApplyADia appColor n = centerXY finalDia where
  trianglePortsCircle = hcat [
    reflectX (apply0Triangle appColor),
    hcat $ take n $ map (\x -> makePort (Port x) <> portCircle <> strutX (circleRadius * 1.5)) [2,3..],
    makePort (Port 1) <> alignR (lc appColor $ lwG defaultLineWidth $ fc appColor $ circle circleRadius)
    ]
  allPorts = makePort (Port 0) <> alignL trianglePortsCircle
  topAndBottomLineWidth = width allPorts - circleRadius
  topAndBottomLine = alignL $ lwG defaultLineWidth $ lc appColor $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine === allPorts === topAndBottomLine

generalTextAppDia :: SpecialBackend b n =>
  Colour Double -> Colour Double -> Int -> String -> TransformableDia b n
generalTextAppDia textCol borderCol numArgs str name _ reflect angle = nameDiagram name rotateDia where
  rotateDia = transformCorrectedTextBox str textCol borderCol reflect angle |||
    coloredApplyADia borderCol numArgs

-- TODO Refactor with generalNestedDia
nestedPAppDia :: SpecialBackend b n =>
  [Colour Double] -> [Maybe (NodeName, Icon)] -> TransformableDia b n
nestedPAppDia borderCols funcNodeNameAndArgs name nestingLevel reflect angle = named name $ case funcNodeNameAndArgs of
  [] -> mempty
  (maybeFunText:args) -> centerXY $ centerY finalDia ||| transformedText ||| resultCircleAndPort
    where
      borderCol = borderCols !! nestingLevel

      transformedText = case maybeFunText of
        Just _ -> makeInnerIcon True 0 maybeFunText
        Nothing -> mempty
      separation = circleRadius * 1.5
      verticalSeparation = circleRadius
      resultCircleAndPort = makeQualifiedPort name (Port 1) <> alignR (lc borderCol $ lwG defaultLineWidth $ fc borderCol $ circle circleRadius)
      triangleAndPorts = vsep separation $
        rotate quarterTurn (apply0Triangle borderCol) :
        zipWith (makeInnerIcon False) [2,3..] args

  
      allPorts = makeQualifiedPort name (Port 0) <> alignT triangleAndPorts -- alignL (strutX separation ||| trianglePortsCircle)
      topAndBottomLineWidth = width allPorts
      -- boxHeight = height 
      argBox = alignT $ lwG defaultLineWidth $ lc borderCol $ roundedRect topAndBottomLineWidth (height allPorts + verticalSeparation) (circleRadius * 0.5)
      finalDia = argBox <> allPorts

      makeInnerIcon _ portNum Nothing = makeQualifiedPort name (Port portNum) <> portCircle
      makeInnerIcon True _ (Just (_, TextBoxIcon t)) = transformCorrectedTextBox t (textBoxTextC colorScheme) borderCol reflect angle
      makeInnerIcon func _ (Just (iconNodeName, icon)) = iconToDiagram icon iconNodeName innerLevel reflect angle where
        innerLevel = if func then nestingLevel else nestingLevel + 1
  

generalNestedDia :: SpecialBackend b n =>
  (Colour Double -> SpecialQDiagram b n) -> [Colour Double] -> [Maybe (NodeName, Icon)] -> TransformableDia b n
generalNestedDia dia borderCols funcNodeNameAndArgs name nestingLevel reflect angle = named name $ case funcNodeNameAndArgs of
  [] -> mempty
  (maybeFunText:args) -> centerXY $  transformedText ||| centerY finalDia
    where
      borderCol = borderCols !! nestingLevel

      transformedText = case maybeFunText of
        Just _ -> makeInnerIcon True 0 maybeFunText
        Nothing -> mempty
      seperation = circleRadius * 1.5
      verticalSeperation = circleRadius
      trianglePortsCircle = hsep seperation $
        reflectX (dia borderCol) :
        zipWith (makeInnerIcon False) [2,3..] args ++
        [makeQualifiedPort name (Port 1) <> alignR (lc borderCol $ lwG defaultLineWidth $ fc borderCol $ circle circleRadius)]
  
      allPorts = makeQualifiedPort name (Port 0) <> alignL trianglePortsCircle
      topAndBottomLineWidth = width allPorts - circleRadius
      argBox = alignL $ lwG defaultLineWidth $ lc borderCol $ roundedRect topAndBottomLineWidth (height allPorts + verticalSeperation) (circleRadius * 0.5)
      finalDia = argBox <> allPorts

      makeInnerIcon _ portNum Nothing = makeQualifiedPort name (Port portNum) <> portCircle
      makeInnerIcon True _ (Just (_, TextBoxIcon t)) = transformCorrectedTextBox t (textBoxTextC colorScheme) borderCol reflect angle
      makeInnerIcon func _ (Just (iconNodeName, icon)) = iconToDiagram icon iconNodeName innerLevel reflect angle where
        innerLevel = if func then nestingLevel else nestingLevel + 1

nestedApplyDia :: SpecialBackend b n =>
  LikeApplyFlavor -> [Maybe (NodeName, Icon)] -> TransformableDia b n
nestedApplyDia flavor = case flavor of
  ApplyNodeFlavor -> generalNestedDia apply0Triangle (nestingC colorScheme)
  ComposeNodeFlavor -> generalNestedDia composeSemiCircle (repeat $ apply1C colorScheme)

-- END Apply like diagrams

-- BEGIN Text boxes and icons --

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 1
monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.61
textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.1

-- BEGIN Text helper functions --

-- This may be a faster implementation of normalizeAngle
--Get the decimal part of a float
-- reduceAngleRange :: SpecialNum a => a -> a
-- reduceAngleRange x = x - fromInteger (floor x)

-- | Given the number of letters in a textbox string, make a rectangle that will
-- enclose the text box. Since the normal SVG text has no size, some hackery is
-- needed to determine the size of the text's bounding box.
rectForText :: (InSpace V2 n t, TrailLike t) => Int -> t
rectForText n = rect rectangleWidth (textBoxFontSize * textBoxHeightFactor)
  where
    rectangleWidth = fromIntegral n * textBoxFontSize * monoLetterWidthToHeightFraction
      + (textBoxFontSize * 0.2)

-- END Text helper functions

commentTextArea :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
commentTextArea textColor t =
  alignL $ fontSize (local textBoxFontSize) (font "freemono" $ fc textColor $ topLeftText t)
  <>  alignTL (lw none $ rectForText (length t))

multilineComment :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment textColor boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (commentTextArea textColor) textLines
    textDia = vcat textAreas

coloredTextBox :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor boxColor t =
  fontSize (local textBoxFontSize) (bold $ font "freemono" $ fc textColor $ text t)
  <>  lwG (0.6 * defaultLineWidth) (lcA boxColor $ fcA (withOpacity (backgroundC colorScheme) 0.5) $ rectForText (length t))

bindTextBox :: SpecialBackend b n =>
  String -> SpecialQDiagram b n
bindTextBox = coloredTextBox (bindTextBoxTextC colorScheme) $ opaque (bindTextBoxC colorScheme)

transformCorrectedTextBox :: SpecialBackend b n =>
  String -> Colour Double -> Colour Double -> Bool -> Angle n -> SpecialQDiagram b n
transformCorrectedTextBox str textCol borderCol reflect angle =
  rotateBy textBoxRotation (reflectIfTrue reflect (coloredTextBox textCol (opaque borderCol) str))
  where
    -- If normalizeAngle is slow, the commented out function reduceAngleRange might be faster
    reducedAngle = normalizeAngle angle ^. turn
    textBoxRotation = if (reducedAngle > (1/4)) && (reducedAngle < (3/4)) then 1 / 2 else 0
    reflectIfTrue shouldReflect dia = if shouldReflect then reflectX dia else dia

textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t name _ reflect angle = nameDiagram name $ transformCorrectedTextBox t (textBoxTextC colorScheme) (textBoxC colorScheme) reflect angle

-- END Text boxes and icons

-- BEGIN Guard and case icons --
guardSize :: (Fractional a) => a
guardSize = 0.7

guardTriangle :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
guardTriangle portDia =
  alignL $ alignR (triangleAndPort ||| lwG defaultLineWidth (hrule (guardSize * 0.8))) <> portDia
  where
    triangleAndPort = alignR $ alignT $ lwG defaultLineWidth $ rotateBy (1/8) $
      polygon (polyType .~ PolySides [90 @@ deg, 45 @@ deg] [guardSize, guardSize] $ with)

-- | generalNestedGuard port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedGuard :: SpecialBackend b n =>
  Colour Double -> (SpecialQDiagram b n -> SpecialQDiagram b n) -> SpecialQDiagram b n -> [Maybe (NodeName, Icon)] -> TransformableDia b n
generalNestedGuard triangleColor lBracket bottomDia inputAndArgs name nestingLevel reflect angle = named name $ case inputAndArgs of
  [] -> mempty
  input : args -> centerXY finalDia where
    finalDia = alignT (bottomDia <> makeQualifiedPort name (Port 1)) <> alignB (inputIcon === (bigVerticalLine <> guardDia <> makeQualifiedPort name (Port 0)))

    argPortNums = [2..]

    iconMapper portNum arg
      | even portNum = Right $ guardTriangle port ||| makeInnerIcon True arg
      | otherwise = Left $ makeInnerIcon False arg ||| lBracket port
      where
        port = makeQualifiedPort name (Port portNum)

    (lBrackets, trianglesWithPorts) = partitionEithers $ zipWith iconMapper argPortNums args

    trianglesAndBrackets =
      zipWith zipper trianglesWithPorts lBrackets

    zipper thisTriangle lBrack = verticalLine === (alignR (extrudeRight guardSize lBrack) <> lc triangleColor (alignL thisTriangle))
      where
        verticalLine = strutY 0.4

    inputIcon = makeInnerIcon False input
    
    guardDia = vcat (alignT trianglesAndBrackets)
    bigVerticalLine = alignT $ lwG defaultLineWidth $ lc triangleColor $ vrule (height guardDia)

    makeInnerIcon innerReflected mNameAndIcon = case mNameAndIcon of
      Nothing -> mempty
      Just (iconNodeName, icon) -> if innerReflected
        then reflectX dia
        else dia
        where
          dia = iconToDiagram icon iconNodeName nestingLevel (innerReflected /= reflect) angle

guardLBracket :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
guardLBracket portDia = alignL (alignT ell) <> portDia
  where
    ellShape = fromOffsets $ map r2 [(0, guardSize), (-guardSize,0)]
    ell = lineJoin LineJoinRound $ lwG defaultLineWidth $ lc (boolC colorScheme) (strokeLine ellShape)

-- | The ports of the guard icon are as follows:
-- Port 0: Top result port (not used)
-- Port 1: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
nestedGuardDia :: SpecialBackend b n => [Maybe (NodeName, Icon)] -> TransformableDia b n
nestedGuardDia = generalNestedGuard lineCol guardLBracket mempty

-- TODO Improve design to be more than a circle.
caseResult :: SpecialBackend b n =>
  SpecialQDiagram b n
caseResult = lw none $ lc caseCColor $ fc caseCColor $ circle circleRadius
  where
    caseCColor = caseRhsC colorScheme

caseC :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
caseC portDia = caseResult <> portDia

-- | The ports of the case icon are as follows:
-- Port 0: Top input port
-- Port 1: Bottom result port
-- Ports 3,5...: The left ports for the results
-- Ports 2,4...: The right ports for the patterns
nestedCaseDia :: SpecialBackend b n => [Maybe (NodeName, Icon)] -> TransformableDia b n
nestedCaseDia = generalNestedGuard (patternC colorScheme) caseC caseResult

-- END Guard and case icons

-- Lambda icon --
-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
flatLambda :: SpecialBackend b n => Int -> SpecialQDiagram b n
flatLambda n = finalDia where
  lambdaCircle = lwG defaultLineWidth $ lc (regionPerimC colorScheme) $ fc (regionPerimC colorScheme) $ circle circleRadius
  lambdaParts = (makePort (Port 0) <> resultIcon) : (portIcons ++  [makePort (Port 1) <> alignR lambdaCircle])
  portIcons = take n $ map (\x -> makePort (Port x) <> portCircle) [2,3..]
  middle = alignL (hsep 0.5 lambdaParts)
  topAndBottomLineWidth = width middle - circleRadius
  topAndBottomLine = alignL $ lwG defaultLineWidth $ lc (regionPerimC colorScheme) $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine <> alignB (topAndBottomLine <> alignT middle)

-- END Main icons
-- END Icons
