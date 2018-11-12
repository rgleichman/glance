{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
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
    guardRhsPorts,
    guardBoolPorts,
    textBox,
    multilineComment,
    defaultLineWidth,
    ColorStyle(..),
    colorScheme,
    coloredTextBox,
    circleRadius
    ) where

import Diagrams.Prelude hiding ((&), (#), Name)

import qualified Control.Arrow as Arrow
import Data.Either(partitionEithers)
import Data.List(find)
import Data.Maybe(catMaybes, listToMaybe, isJust, fromJust)
import Data.Typeable(Typeable)

import Types(Icon(..), SpecialQDiagram, SpecialBackend, SpecialNum
            , NodeName, Port(..), LikeApplyFlavor(..),
            SyntaxNode(..), NamedIcon(..), Labeled(..))
import DrawingColors(colorScheme, ColorStyle(..))

{-# ANN module "HLint: ignore Use record patterns" #-}

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

iconToDiagram :: SpecialBackend b n => Icon -> TransformableDia b n
iconToDiagram icon = case icon of
  TextBoxIcon s -> textBox s
  BindTextBoxIcon s -> identDiaFunc $ bindTextBox s
  GuardIcon n -> nestedGuardDia $ replicate (1 + (2 * n)) Nothing
  CaseIcon n -> nestedCaseDia $ replicate (1 + (2 * n)) Nothing
  CaseResultIcon -> identDiaFunc caseResult
  FlatLambdaIcon x _ -> flatLambda x
  NestedApply flavor headIcon args -> nestedApplyDia flavor headIcon args
  NestedPApp constructor args
    -> nestedPAppDia (repeat $ patternC colorScheme) constructor args
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
  NestedApply _ headIcon args -> snd <$> findIcon name (headIcon : args)
  NestedPApp constructor args ->
    snd <$> findIcon name (fmap laValue (constructor:args))
  _ -> Nothing

findIcon :: NodeName -> [Maybe NamedIcon] -> Maybe (Int, Icon)
findIcon name args = icon where
  numberedArgs = zip ([0,1..] :: [Int]) args
  filteredArgs = Arrow.second fromJust <$> filter (isJust . snd) numberedArgs
  nameMatches (_, NamedIcon n _) = n == name
  icon = case find nameMatches filteredArgs of
    Nothing -> listToMaybe $ catMaybes $ fmap findSubSubIcon filteredArgs
    Just (argNum, NamedIcon _ finalIcon) -> Just (argNum, finalIcon)
    where
      findSubSubIcon (argNum, NamedIcon _ subIcon)
        = case findNestedIcon name subIcon of
            Nothing -> Nothing
            Just x -> Just (argNum, x)

generalNestedPortAngles :: SpecialNum n
                        => (Port -> [Angle n])
                        -> Maybe NamedIcon
                        -> [Maybe NamedIcon]
                        -> Port -> Maybe NodeName -> [Angle n]
generalNestedPortAngles defaultAngles headIcon args port maybeNodeName =
  case maybeNodeName of
    Nothing -> defaultAngles port
    Just name -> case findIcon name (headIcon : args) of
      Nothing -> []
      Just (_, icon) -> getPortAngles icon port Nothing

reflectXAngle :: SpecialNum n => Angle n -> Angle n
reflectXAngle x = reflectedAngle where
  normalizedAngle = normalizeAngle x
  reflectedAngle = (-) <$> halfTurn <*> normalizedAngle

-- TODO reflect the angles for the right side sub-icons
nestedGuardPortAngles :: SpecialNum n =>
  [Maybe NamedIcon]
  -> Port
  -> Maybe NodeName
  -> [Angle n]
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
  TextBoxIcon _ -> []
  BindTextBoxIcon _ -> []
  GuardIcon _ -> guardPortAngles port
  CaseIcon _ -> guardPortAngles port
  CaseResultIcon -> []
  FlatLambdaIcon _ _ -> applyPortAngles port
  NestedApply _ headIcon args ->
    generalNestedPortAngles applyPortAngles headIcon args port maybeNodeName
  NestedPApp headIcon args ->
    generalNestedPortAngles
    pAppPortAngles (laValue headIcon) (fmap laValue args) port maybeNodeName
  NestedCaseIcon args -> nestedGuardPortAngles args port maybeNodeName
  NestedGuardIcon args -> nestedGuardPortAngles args port maybeNodeName

-- END getPortAngles --

-- BEGIN Port numbers

inputPortConst :: Port
inputPortConst = Port 0

resultPortConst :: Port
resultPortConst = Port 1

argPortsConst :: [Port]
argPortsConst = fmap Port [2,3..]

-- TODO It's a bit strange that the parameter is a SyntaxNode, not an Icon.
inputPort :: SyntaxNode -> Port
inputPort = const inputPortConst

resultPort :: SyntaxNode -> Port
resultPort = const resultPortConst

caseRhsPorts :: [Port]
caseRhsPorts = fmap Port [3,5..]

casePatternPorts :: [Port]
casePatternPorts = fmap Port [2,4..]

guardRhsPorts :: [Port]
guardRhsPorts = casePatternPorts

guardBoolPorts :: [Port]
guardBoolPorts = caseRhsPorts

argumentPorts :: SyntaxNode -> [Port]
argumentPorts n = case n of
  LikeApplyNode  _ _-> defaultPorts
  NestedApplyNode _ _ _ -> defaultPorts
  NestedPatternApplyNode _ _-> defaultPorts
  FunctionDefNode _ _ -> defaultPorts
  NestedCaseOrGuardNode _ _ _-> defaultPorts
  GuardNode _ -> defaultPorts
  CaseNode _ -> defaultPorts
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
  TransformParams n ->
  Bool ->
  Port ->
  Labeled (Maybe NamedIcon) ->
  SpecialQDiagram b n
makeAppInnerIcon (TransformParams name _ reflect angle) _ portNum
  (Labeled Nothing str)
  = centerX $ makeLabelledPort name reflect angle str portNum
makeAppInnerIcon (TransformParams _ nestingLevel reflect angle) func _
  (Labeled (Just (NamedIcon iconNodeName icon)) _)
  = iconToDiagram
    icon
    (TransformParams iconNodeName innerLevel reflect angle)
  where
    innerLevel = if func then nestingLevel else nestingLevel + 1

makeTransformedText :: SpecialBackend b n =>
  TransformParams n
  -> Labeled (Maybe NamedIcon)
  -> SpecialQDiagram b n
makeTransformedText tp maybeFunText = case laValue maybeFunText of
  Just _ ->
    makeAppInnerIcon tp True inputPortConst maybeFunText
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

nestedPAppDia :: SpecialBackend b n =>
  [Colour Double]
  -> Labeled (Maybe NamedIcon)
  -> [Labeled (Maybe NamedIcon)]
  -> TransformableDia b n
nestedPAppDia
  borderCols
  maybeFunText
  args
  tp@(TransformParams name nestingLevel _ _)
  = named name $ centerXY
    $ centerY finalDia ||| beside' unitX transformedText resultCircleAndPort
  where
    borderCol = borderCols !! nestingLevel
    transformedText = makeTransformedText tp maybeFunText
    separation = circleRadius * 1.5
    resultCircleAndPort
      = makeQualifiedPort name resultPortConst
        <> alignR
        (lc borderCol
          $ lwG defaultLineWidth $ fc borderCol $ circle circleRadius)
    triangleAndPorts
      = vsep separation $
        rotate quarterTurn (apply0Triangle borderCol) :
        zipWith (makeAppInnerIcon tp False) argPortsConst args
    allPorts
      = makeQualifiedPort name inputPortConst <> alignT triangleAndPorts
    argBox = alignT $ appArgBox
             borderCol
             (width allPorts)
             (height allPorts)
    finalDia = argBox <> allPorts


-- | Like beside, but it puts the second dia atop the first dia
beside' :: (Semigroup a, Juxtaposable a) => V a (N a) -> a -> a -> a
beside' dir dia1 dia2 = juxtapose dir dia1 dia2 <> dia1

-- | apply port locations:
-- inputPortConst: Function
-- resultPortConst: Result
-- Ports 2,3..: Arguments
generalNestedDia :: SpecialBackend b n
                 => (Colour Double -> SpecialQDiagram b n)
                 -> [Colour Double]
                 -> Maybe NamedIcon
                 -> [Maybe NamedIcon]
                 -> TransformableDia b n
generalNestedDia
  dia
  borderCols
  maybeFunText
  args
  tp@(TransformParams name nestingLevel _ _)
  = named name $ centerXY $ beside' unitX transformedText finalDia
    where
      borderCol = borderCols !! nestingLevel
      transformedText = makeTransformedText tp (pure maybeFunText)
      separation = circleRadius * 1.5
      trianglePortsCircle = hsep separation $
        reflectX (dia borderCol) :
        zipWith (makeAppInnerIcon tp False) argPortsConst (fmap pure args) ++
        [makeQualifiedPort name resultPortConst
         <> alignR
          (lc borderCol $ lwG defaultLineWidth $ fc borderCol
            $ circle circleRadius)
        ]
      allPorts
        = makeQualifiedPort name inputPortConst <> alignL trianglePortsCircle
      argBox = alignL $ appArgBox
               borderCol
               (width allPorts - circleRadius)
               (height allPorts)
      finalDia = argBox <> allPorts


nestedApplyDia :: SpecialBackend b n
  => LikeApplyFlavor
  -> Maybe NamedIcon
  -> [Maybe NamedIcon]
  -> TransformableDia b n
nestedApplyDia flavor = case flavor of
  ApplyNodeFlavor -> generalNestedDia apply0Triangle (nestingC colorScheme)
  ComposeNodeFlavor ->
    generalNestedDia composeSemiCircle (repeat $ apply1C colorScheme)

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

-- BEGIN Guard and case icons --
guardSize :: (Fractional a) => a
guardSize = 0.7

guardTriangle :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
guardTriangle portDia =
  alignL
  $ alignR (triangleAndPort ||| lwG defaultLineWidth (hrule (guardSize * 0.8)))
  <> portDia
  where
    triangleAndPort = alignR $ alignT $ lwG defaultLineWidth $ rotateBy (1/8) $
      polygon
      (polyType .~ PolySides [90 @@ deg, 45 @@ deg] [guardSize, guardSize]
       $ with)

-- | generalNestedGuard port layout:
-- 0 -> top
-- 1 -> bottom
-- odds -> left
-- evens -> right
generalNestedGuard :: SpecialBackend b n
                   => Colour Double
                   -> (SpecialQDiagram b n -> SpecialQDiagram b n)
                   -> SpecialQDiagram b n
                   -> [Maybe NamedIcon]
                   -> TransformableDia b n
generalNestedGuard triangleColor lBracket bottomDia inputAndArgs
  (TransformParams name nestingLevel reflect angle)
  = named name $ case inputAndArgs of
  [] -> mempty
  input : args -> centerXY finalDia where
    finalDia = alignT (bottomDia <> makeQualifiedPort name resultPortConst)
               <> alignB
               (inputIcon === (bigVerticalLine
                               <> guardDia
                               <> makeQualifiedPort name inputPortConst))

    iconMapper (Port portNum) arg
      | even portNum = Right $ guardTriangle port ||| makeInnerIcon True arg
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
        (alignR (extrudeRight guardSize lBrack)
         <> lc triangleColor (alignL thisTriangle))
      where
        verticalLine = strutY 0.4

    inputIcon = makeInnerIcon False input

    guardDia = vcat (alignT trianglesAndBrackets)
    bigVerticalLine
      = alignT
        $ lwG defaultLineWidth $ lc triangleColor $ vrule (height guardDia)

    makeInnerIcon innerReflected mNameAndIcon = case mNameAndIcon of
      Nothing -> mempty
      Just (NamedIcon iconNodeName icon) -> if innerReflected
        then reflectX dia
        else dia
        where
          dia = iconToDiagram icon (TransformParams
                                     iconNodeName
                                     nestingLevel
                                     (innerReflected /= reflect)
                                     angle)

guardLBracket :: SpecialBackend b n =>
  SpecialQDiagram b n -> SpecialQDiagram b n
guardLBracket portDia = alignL (alignT ell) <> portDia
  where
    ellShape = fromOffsets $ map r2 [(0, guardSize), (-guardSize, 0)]
    ell
      = lineJoin LineJoinRound
      $ lwG defaultLineWidth $ lc (boolC colorScheme) (strokeLine ellShape)

-- | The ports of the guard icon are as follows:
-- inputPortConst: Top result port (not used)
-- resultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the booleans
-- Ports 2,4...: The right ports for the values
nestedGuardDia :: SpecialBackend b n =>
  [Maybe NamedIcon]
  -> TransformableDia b n
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
-- inputPortConst: Top input port
-- resultPortConst: Bottom result port
-- Ports 3,5...: The left ports for the results
-- Ports 2,4...: The right ports for the patterns
nestedCaseDia :: SpecialBackend b n => [Maybe NamedIcon] -> TransformableDia b n
nestedCaseDia = generalNestedGuard (patternC colorScheme) caseC caseResult

-- END Guard and case icons

-- | The ports of flatLambdaIcon are:
-- 0: Result icon
-- 1: The lambda function value
-- 2,3.. : The parameters
flatLambda :: SpecialBackend b n => [String] -> TransformableDia b n
flatLambda paramNames (TransformParams name _ reflect angle)
  = named name finalDia
  where
  lambdaCircle
    = lwG defaultLineWidth
      $ lc (regionPerimC colorScheme)
      $ fc (regionPerimC colorScheme) $ circle (1.5 * circleRadius)
  lambdaParts
    = (makeQualifiedPort name inputPortConst <> resultIcon)
      :
      (portIcons
        ++ [makeQualifiedPort name resultPortConst <> alignR lambdaCircle])

  portIcons
    = zipWith (makeLabelledPort name reflect angle) paramNames argPortsConst
  middle = alignL (hsep 0.5 lambdaParts)
  topAndBottomLineWidth = width middle - circleRadius
  topAndBottomLine
    = alignL
      $ lwG defaultLineWidth
      $ lc (regionPerimC colorScheme) $ hrule topAndBottomLineWidth
  finalDia = topAndBottomLine <> alignB (topAndBottomLine <> alignT middle)

-- END Main icons
-- END Icons
