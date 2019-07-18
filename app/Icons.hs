{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icons
    (
    TransformParams(..),
    TransformableDia,
    getPortAngles,
    iconToDiagram,
    inputPort,
    resultPort,
    argumentPorts,
    caseRhsPorts,
    casePatternPorts,
    multiIfRhsPorts,
    multiIfBoolPorts,
    textBox,
    multilineComment,
    defaultLineWidth,
    ColorStyle(..),
    colorScheme,
    coloredTextBox,
    circleRadius,
    findIconFromName
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Control.Arrow as Arrow
import Data.Either(partitionEithers)
import qualified Data.IntMap as IM
import Data.List(find)
import Data.Maybe(listToMaybe, isJust, fromJust, mapMaybe)
import Data.Typeable(Typeable)

import Constants(pattern InputPortConst, pattern ResultPortConst)
import DrawingColors(colorScheme, ColorStyle(..))
import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName(..), Port(..), LikeApplyFlavor(..),
            SyntaxNode(..), NamedIcon, Labeled(..), IconInfo
            , Named(..))

{-# ANN module "HLint: ignore Use record patterns" #-}
{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- TYPES --

data TransformParams n = TransformParams {
  tpName :: NodeName  -- The icon's name
  , tpNestingDepth :: Int  -- The icon's nesting depth
  , tpIsReflected :: Bool  -- If the icon will be reflected
  , tpAngle :: Angle n  -- By what angle will the icon be rotated
  }

-- | A TransformableDia is a function that returns a diagram for an icon when
-- given the icon's name, its nesting depth, whether it will be reflected, and
-- by what angle it will be rotated.
type TransformableDia b n = TransformParams n -> SpecialQDiagram b n


-- CONSTANTS --
defaultLineWidth :: (Fractional a) => a
defaultLineWidth = 0.15

circleRadius :: (Fractional a) => a
circleRadius = 0.5

-- COLORS --
lineCol :: Colour Double
lineCol = lineC colorScheme


-- BEGIN Exported icon functions --

findIconFromName :: IconInfo -> NodeName -> NamedIcon
findIconFromName icons name@(NodeName nameInt)
  = Named name $ IM.findWithDefault
    (error $ "findIconFromName: icon not found.\nicons="
      <> show icons <> "\nname=" <> show name)
    nameInt
    icons

-- TODO Detect if we are in a loop (have called iconToDiagram on the same node
-- before)
iconToDiagram :: SpecialBackend b n
  => IconInfo
  -> Icon
  -> TransformableDia b n
iconToDiagram iconInfo icon = case icon of
  TextBoxIcon s -> textBox s
  BindTextBoxIcon s -> identDiaFunc $ bindTextBox s
  MultiIfIcon n -> nestedMultiIfDia iconInfo $ replicate (1 + (2 * n)) Nothing
  CaseIcon n -> nestedCaseDia iconInfo $ replicate (1 + (2 * n)) Nothing
  CaseResultIcon -> identDiaFunc caseResult
  LambdaIcon x bodyExp _
    -> nestedLambda iconInfo x (findIconFromName iconInfo <$> bodyExp)
  NestedApply flavor headIcon args
    -> nestedApplyDia
       iconInfo
       flavor
       (fmap (findIconFromName iconInfo) headIcon)
       ((fmap . fmap) (findIconFromName iconInfo) args)
  NestedPApp constructor args
    -> nestedPAppDia iconInfo (repeat $ patternC colorScheme) constructor args
  NestedCaseIcon args -> nestedCaseDia iconInfo args
  NestedMultiIfIcon args -> nestedMultiIfDia iconInfo args

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

multiIfPortAngles :: Floating n => Port -> [Angle n]
multiIfPortAngles (Port port) = case port of
  0 -> [1/4 @@ turn]
  1 -> [3/4 @@ turn]
  _ -> otherAngles where otherAngles
                           | even port = [0 @@ turn]
                           | otherwise = [1/2 @@ turn]

findNestedIcon :: IconInfo -> NodeName -> Icon -> Maybe Icon
findNestedIcon iconInfo name icon = case icon of
  NestedApply _ headIcon args
    -> snd
       <$> findIcon
       iconInfo
       name
       ((fmap . fmap) (findIconFromName iconInfo) (headIcon : args))
  NestedPApp constructor args ->
    snd <$> findIcon iconInfo name (fmap laValue (constructor:args))
  _ -> Nothing

findIcon :: IconInfo -> NodeName -> [Maybe NamedIcon] -> Maybe (Int, Icon)
findIcon iconInfo name args = icon where
  numberedArgs = zip ([0,1..] :: [Int]) args
  filteredArgs = Arrow.second fromJust <$> filter (isJust . snd) numberedArgs
  nameMatches (_, Named n _) = n == name
  icon = case find nameMatches filteredArgs of
    Nothing -> listToMaybe $ mapMaybe findSubSubIcon filteredArgs
    Just (argNum, Named _ finalIcon) -> Just (argNum, finalIcon)
    where
      findSubSubIcon (argNum, Named _ subIcon)
        = case findNestedIcon iconInfo name subIcon of
            Nothing -> Nothing
            Just x -> Just (argNum, x)

generalNestedPortAngles :: SpecialNum n
  => IconInfo
  -> (Port -> [Angle n])
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> Port -> Maybe NodeName -> [Angle n]
generalNestedPortAngles iconInfo defaultAngles headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngles port
    Just name -> case findIcon iconInfo name (headIcon : args) of
      Nothing -> []
      Just (_, icon) -> getPortAngles iconInfo icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

-- TODO reflect the angles for the right side sub-icons
nestedMultiIfPortAngles :: SpecialNum n
  => IconInfo
  -> [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> [Angle n]
nestedMultiIfPortAngles iconInfo args port maybeNodeName = case maybeNodeName of
  Nothing -> multiIfPortAngles port
  Just name -> case findIcon iconInfo name args of
    Nothing -> []
    -- TODO Don't use hardcoded numbers
    -- The arguments correspond to ports [0, 2, 3, 4 ...]
    Just (argNum, icon) -> if odd argNum && argNum >= 1
      -- The icon will be reflected
      then fmap reflectXAngle subAngles
      else subAngles
      where
        subAngles = getPortAngles iconInfo icon port Nothing

getPortAngles :: SpecialNum n => IconInfo -> Icon -> Port -> Maybe NodeName -> [Angle n]
getPortAngles iconInfo icon port maybeNodeName = case icon of
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  MultiIfIcon _ -> multiIfPortAngles port
  CaseIcon _ -> multiIfPortAngles port
  CaseResultIcon -> []
  LambdaIcon _ _ _ -> applyPortAngles port
  NestedApply _ headIcon args
    -> generalNestedPortAngles
       iconInfo
       applyPortAngles
       -- TODO Refactor with iconToDiagram
       (fmap (findIconFromName iconInfo) headIcon)
       ((fmap . fmap) (findIconFromName iconInfo) args)
       port
       maybeNodeName
  NestedPApp headIcon args
    -> generalNestedPortAngles
       iconInfo
       pAppPortAngles
       (laValue headIcon)
       (fmap laValue args)
       port
       maybeNodeName
  NestedCaseIcon args
    -> nestedMultiIfPortAngles iconInfo args port maybeNodeName
  NestedMultiIfIcon args
    -> nestedMultiIfPortAngles iconInfo args port maybeNodeName

-- END getPortAngles --

-- BEGIN Port numbers

argPortsConst :: [Port]
argPortsConst = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const InputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const ResultPortConst

caseRhsPorts :: [Port]
caseRhsPorts = fmap Port [3,5..]

casePatternPorts :: [Port]
casePatternPorts = fmap Port [2,4..]

multiIfRhsPorts :: [Port]
multiIfRhsPorts = casePatternPorts

multiIfBoolPorts :: [Port]
multiIfBoolPorts = caseRhsPorts

argumentPorts :: SyntaxNode -> [Port]
argumentPorts n = case n of
  (ApplyNode _ _) -> defaultPorts
  PatternApplyNode _ _-> defaultPorts
  (FunctionDefNode _ _) -> defaultPorts
  CaseOrMultiIfNode _ _ _-> defaultPorts
  NameNode _ -> []
  BindNameNode _ -> []
  LiteralNode _ -> []
  CaseResultNode -> []
  where
    defaultPorts = argPortsConst
-- END Port numbers

-- END Exported icon functions --

-- BEGIN Diagram helper functions --

-- | Make an identity TransformableDia
identDiaFunc :: SpecialNum n => SpecialQDiagram b n -> TransformableDia b n
identDiaFunc dia transformParams = nameDiagram (tpName transformParams) dia

-- | Names the diagram and puts all sub-names in the namespace of the top level
-- name.
nameDiagram :: SpecialNum n =>
  NodeName
  -> SpecialQDiagram b n
  -> SpecialQDiagram b n
nameDiagram name dia = named name (name .>> dia)

-- | Make an port with an integer name. Always use <> to add a ports
-- (not === or |||)  since mempty has no size and will not be placed where you
-- want it.
makePort :: SpecialNum n => Port -> SpecialQDiagram b n
makePort x = named x mempty
--makePort x = circle 0.2 # fc green # named x
-- Note, the version of makePort below seems to have a different type.
--makePort x = textBox (show x) # fc green # named x

makeQualifiedPort :: SpecialNum n => NodeName -> Port -> SpecialQDiagram b n
makeQualifiedPort n x = n .>> makePort x

makeLabelledPort :: SpecialBackend b n =>
  NodeName -> Bool -> Angle n -> String -> Port -> SpecialQDiagram b n
makeLabelledPort name reflect angle str portNum = case str of
  -- Don't display " tempvar" from Translate.hs/matchesToCase
  (' ':_) -> portAndCircle
  (_:_:_) -> portAndCircle ||| label
  _ -> portAndCircle
  where
    portAndCircle = makeQualifiedPort name portNum <> portCircle
    label = transformableBindTextBox str reflect angle

-- END Diagram helper functions


-- BEGIN Icons --

-- BEGIN Sub-diagrams --

apply0Triangle :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
apply0Triangle col
  = fc col $ lw none $ rotateBy (-1/12) $ eqTriangle (2 * circleRadius)

composeSemiCircle :: SpecialBackend b n => Colour Double -> SpecialQDiagram b n
composeSemiCircle col
  = lc col $ lwG defaultLineWidth $ wedge circleRadius yDir halfTurn

portCircle :: SpecialBackend b n => SpecialQDiagram b n
portCircle = lw none $ fc lineCol $ circle (circleRadius * 0.5)

resultIcon :: SpecialBackend b n => SpecialQDiagram b n
resultIcon =  lw none $ fc (lamArgResC colorScheme) unitSquare

-- END Sub-diagrams

-- BEGIN Main icons

-- BEGIN Apply like icons

makeAppInnerIcon :: SpecialBackend b n =>
  IconInfo ->
  TransformParams n ->
  Bool ->  -- If False then add one to the nesting level.
  Port ->  -- Port number (if the NamedIcon is Nothing)
  Labeled (Maybe NamedIcon) ->  -- The icon
  SpecialQDiagram b n
makeAppInnerIcon _iconInfo (TransformParams name _ reflect angle) _ portNum
  (Labeled Nothing str)
  = centerX $ makeLabelledPort name reflect angle str portNum
makeAppInnerIcon iconInfo (TransformParams _ nestingLevel reflect angle) func _
  (Labeled (Just (Named iconNodeName icon)) _)
  = iconToDiagram
    iconInfo
    icon
    (TransformParams iconNodeName innerLevel reflect angle)
  where
    innerLevel = if func then nestingLevel else nestingLevel + 1

makeTransformedText :: SpecialBackend b n
  => IconInfo
  -> TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> SpecialQDiagram b n
makeTransformedText iconInfo tp maybeFunText = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon iconInfo tp True InputPortConst maybeFunText
  Nothing -> mempty

appArgBox :: (HasStyle a, Typeable (N a)
             , TrailLike a, RealFloat (N a), V a ~ V2)
          => Colour Double -> N a -> N a -> a
appArgBox borderCol topAndBottomLineWidth portHeight
  = lwG defaultLineWidth $ lc borderCol
    $ roundedRect
    topAndBottomLineWidth
    (portHeight + verticalSeparation)
    (circleRadius * 0.5)
  where
    verticalSeparation = circleRadius

nestedPAppDia :: SpecialBackend b n
  => IconInfo
  -> [Colour Double]
  -> Labeled (Maybe NamedIcon)
  -> [Labeled (Maybe NamedIcon)]
  -> TransformableDia b n
nestedPAppDia
  iconInfo
  borderCols
  maybeFunText
  args
  tp@(TransformParams name nestingLevel _ _)
  = named name $ centerXY
    $ centerY finalDia ||| beside' unitX transformedText resultCircleAndPort
  where
    borderCol = borderCols !! nestingLevel
    transformedText = makeTransformedText iconInfo tp maybeFunText
    separation = circleRadius * 1.5
    resultCircleAndPort
      = makeQualifiedPort name ResultPortConst
        <> alignR
        (lc borderCol
          $ lwG defaultLineWidth $ fc borderCol $ circle circleRadius)
    triangleAndPorts
      = vsep separation $
        rotate quarterTurn (apply0Triangle borderCol) :
        zipWith (makeAppInnerIcon iconInfo tp False) argPortsConst args
    allPorts
      = makeQualifiedPort name InputPortConst <> alignT triangleAndPorts
    argBox = alignT $ appArgBox
             borderCol
             (width allPorts)
             (height allPorts)
    finalDia = argBox <> allPorts


-- | Like beside, but it puts the second dia atop the first dia
beside' :: (Semigroup a, Juxtaposable a) => V a (N a) -> a -> a -> a
beside' dir dia1 dia2 = juxtapose dir dia1 dia2 <> dia1

-- | apply port locations:
-- InputPortConst: Function
-- ResultPortConst: Result
-- Ports 2,3..: Arguments
generalNestedDia :: SpecialBackend b n
  => IconInfo
  -> (Colour Double -> SpecialQDiagram b n)
  -> [Colour Double]
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
generalNestedDia
  iconInfo
  dia
  borderCols
  maybeFunText
  args
  tp@(TransformParams name nestingLevel _ _)
  = named name $ centerXY $ beside' unitX transformedText finalDia
    where
      borderCol = borderCols !! nestingLevel
      transformedText = makeTransformedText iconInfo tp (pure maybeFunText)
      separation = circleRadius * 1.5
      trianglePortsCircle = hsep separation $
        reflectX (dia borderCol) :
        zipWith (makeAppInnerIcon iconInfo tp False) argPortsConst (fmap pure args) ++
        [makeQualifiedPort name ResultPortConst
         <> alignR
          (lc borderCol $ lwG defaultLineWidth $ fc borderCol
            $ circle circleRadius)
        ]
      allPorts
        = makeQualifiedPort name InputPortConst <> alignL trianglePortsCircle
      argBox = alignL $ appArgBox
               borderCol
               (width allPorts - circleRadius)
               (height allPorts)
      finalDia = argBox <> allPorts


nestedApplyDia :: SpecialBackend b n
  => IconInfo
  -> LikeApplyFlavor
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedApplyDia iconInfo flavor = case flavor of
  ApplyNodeFlavor
    -> generalNestedDia iconInfo apply0Triangle (nestingC colorScheme)
  ComposeNodeFlavor
    -> generalNestedDia iconInfo composeSemiCircle (repeat $ apply1C colorScheme)

-- END Apply like diagrams

-- BEGIN Text boxes and icons --

-- Text constants --
textBoxFontSize :: (Num a) => a
textBoxFontSize = 1

monoLetterWidthToHeightFraction :: (Fractional a) => a
monoLetterWidthToHeightFraction = 0.61

textBoxHeightFactor :: (Fractional a) => a
textBoxHeightFactor = 1.4

textFont :: String
textFont = "monospace"

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
    rectangleWidth
      = (fromIntegral n * textBoxFontSize * monoLetterWidthToHeightFraction)
        + (textBoxFontSize * 0.3)

-- END Text helper functions

commentTextArea :: SpecialBackend b n =>
  Colour Double -> String -> SpecialQDiagram b n
commentTextArea textColor t =
  alignL
  $ fontSize
  (local textBoxFontSize)
  (font textFont $ fc textColor $ topLeftText t)
  <>  alignTL (lw none $ rectForText (length t))

multilineComment :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
multilineComment textColor _boxColor t = lwG (0.6 * defaultLineWidth) textDia
  where
    textLines = lines t
    textAreas = map (commentTextArea textColor) textLines
    textDia = vcat textAreas

coloredTextBox :: SpecialBackend b n =>
  Colour Double
  -> AlphaColour Double -> String -> SpecialQDiagram b n
coloredTextBox textColor boxColor t =
  fontSize
  (local textBoxFontSize)
  (bold $ font textFont $ fc textColor $ text t)
  <> lwG
  (0.6 * defaultLineWidth)
  (lcA boxColor
    $ fcA (withOpacity (backgroundC colorScheme) 0.5) $ rectForText (length t))

transformCorrectedTextBox :: SpecialBackend b n =>
  String
  -> Colour Double
  -> Colour Double
  -> Bool
  -> Angle n
  -> SpecialQDiagram b n
transformCorrectedTextBox str textCol borderCol reflect angle =
  rotateBy
  textBoxRotation
  (reflectIfTrue reflect (coloredTextBox textCol (opaque borderCol) str))
  where
    -- If normalizeAngle is slow, the commented out function reduceAngleRange
    -- might be faster.
    reducedAngle = normalizeAngle angle ^. turn
    textBoxRotation
      = if (reducedAngle > (1/4)) && (reducedAngle < (3/4)) then 1 / 2 else 0
    reflectIfTrue shouldReflect dia
      = if shouldReflect then reflectX dia else dia

transformableBindTextBox :: SpecialBackend b n =>
  String -> Bool -> Angle n -> SpecialQDiagram b n
transformableBindTextBox str
  = transformCorrectedTextBox
    str
    (bindTextBoxTextC colorScheme)
    (bindTextBoxC colorScheme)

bindTextBox :: SpecialBackend b n =>
  String -> SpecialQDiagram b n
bindTextBox
  = coloredTextBox (bindTextBoxTextC colorScheme)
    $ opaque (bindTextBoxC colorScheme)

textBox :: SpecialBackend b n =>
  String -> TransformableDia b n
textBox t (TransformParams name _ reflect angle)
  = nameDiagram name $ transformCorrectedTextBox
    t (textBoxTextC colorScheme) (textBoxC colorScheme) reflect angle

-- END Text boxes and icons

-- BEGIN MultiIf and case icons --
multiIfSize :: (Fractional a) => a
multiIfSize = 0.7

multiIfTriangle :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
multiIfTriangle portDia =
  alignL
  $ alignR (triangleAndPort ||| lwG defaultLineWidth (hrule (multiIfSize * 0.8)))
  <> portDia
  where
    triangleAndPort = alignR $ alignT $ lwG defaultLineWidth $ rotateBy (1/8) $
      polygon
      (polyType .~ PolySides [90 @@ deg, 45 @@ deg] [multiIfSize, multiIfSize]
       $ with)

-- | generalNestedMultiIf port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedMultiIf :: SpecialBackend b n
                   => IconInfo
                   -> Colour Double
                   -> (SpecialQDiagram b n -> SpecialQDiagram b n)
                   -> SpecialQDiagram b n
                   -> [Maybe NamedIcon]
                   -> TransformableDia b n
generalNestedMultiIf iconInfo triangleColor lBracket bottomDia inputAndArgs
  (TransformParams name nestingLevel reflect angle)
  = named name $ case inputAndArgs of
  [] -> mempty
  input : args -> centerXY finalDia where
    finalDia = alignT (bottomDia <> makeQualifiedPort name ResultPortConst)
               <> alignB
               (inputIcon === (bigVerticalLine
                               <> multiIfDia
                               <> makeQualifiedPort name InputPortConst))

    iconMapper (Port portNum) arg
      | even portNum = Right $ multiIfTriangle port ||| makeInnerIcon True arg
      | otherwise = Left $ makeInnerIcon False arg ||| lBracket port
      where
        port = makeQualifiedPort name (Port portNum)

    (lBrackets, trianglesWithPorts)
      = partitionEithers $ zipWith iconMapper argPortsConst args

    trianglesAndBrackets =
      zipWith zipper trianglesWithPorts lBrackets

    zipper thisTriangle lBrack
      = verticalLine
        ===
        (alignR (extrudeRight multiIfSize lBrack)
         <> lc triangleColor (alignL thisTriangle))
      where
        verticalLine = strutY 0.4

    inputIcon = makeInnerIcon False input

    multiIfDia = vcat (alignT trianglesAndBrackets)
    bigVerticalLine
      = alignT
        $ lwG defaultLineWidth $ lc triangleColor $ vrule (height multiIfDia)

    makeInnerIcon innerReflected mNameAndIcon = case mNameAndIcon of
      Nothing -> mempty
      Just (Named iconNodeName icon) -> if innerReflected
        then reflectX dia
        else dia
        where
          dia = iconToDiagram
                iconInfo
                icon
                (TransformParams
                  iconNodeName
                  nestingLevel
                  (innerReflected /= reflect)
                  angle)

multiIfLBracket :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
multiIfLBracket portDia = alignL (alignT ell) <> portDia
  where
    ellShape = fromOffsets $ map r2 [(0, multiIfSize), (-multiIfSize, 0)]
    ell
      = lineJoin LineJoinRound
      $ lwG defaultLineWidth $ lc (boolC colorScheme) (strokeLine ellShape)

-- | The ports of the multiIf icon are as follows:
-- InputPortConst: Top result port (not used)
-- ResultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
nestedMultiIfDia :: SpecialBackend b n =>
  IconInfo
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedMultiIfDia iconInfo = generalNestedMultiIf iconInfo lineCol multiIfLBracket mempty

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
-- InputPortConst: Top input port
-- ResultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the results
-- Ports 2,4...: The right ports for the patterns
nestedCaseDia :: SpecialBackend b n
  => IconInfo
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedCaseDia iconInfo
  = generalNestedMultiIf iconInfo (patternC colorScheme) caseC caseResult

-- END MultiIf and case icons

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
nestedLambda :: SpecialBackend b n
           => IconInfo
           -> [String]
           -> Maybe NamedIcon
           -> TransformableDia b n
nestedLambda iconInfo paramNames mBodyExp (TransformParams name level reflect angle)
  = centerXY $ bodyExpIcon ||| centerY (named name finalDia)
  where
  lambdaCircle
    = lwG defaultLineWidth
      $ lc (regionPerimC colorScheme)
      $ fc (regionPerimC colorScheme) $ circle (1.5 * circleRadius)
  lambdaParts
    = (makeQualifiedPort name InputPortConst <> resultIcon)
      :
      (portIcons
        ++ [makeQualifiedPort name ResultPortConst <> alignR lambdaCircle])
  bodyExpIcon = case mBodyExp of
    Nothing -> mempty
    Just (Named bodyNodeName bodyIcon)
      -> iconToDiagram
         iconInfo
         bodyIcon
         (TransformParams bodyNodeName level reflect angle)

  portIcons
    = zipWith (makeLabelledPort name reflect angle) paramNames argPortsConst
  middle = alignL (hsep 0.5 lambdaParts)
  topAndBottomLineWidth = width middle - (circleRadius + defaultLineWidth)
  topAndBottomLine
    = alignL
      $ lwG defaultLineWidth
      $ lc (regionPerimC colorScheme) $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine <> alignB (topAndBottomLine <> alignT middle)

-- END Main icons
-- END Icons
