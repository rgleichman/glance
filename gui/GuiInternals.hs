-- This file is formatted with Ormolu
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module is the core of the GUI model and control. This module
-- does not interact with GUI libraries.
module GuiInternals
  ( Inputs (..),
    emptyInputs,
    AppState (..),
    emptyAppState,
    MouseButton (..),
    KeyInput (..),
    KeyEvent (..),
    renderCairo,
    updateBackground,
    backgroundPress,
    keyPress,
    keyRelease,
    scroll,
    step,
  )
where

-- import Debug.Trace (trace)

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Coerce (Coercible)
import Data.Foldable (traverse_)
import Data.GI.Base (withManagedPtr)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock.System (SystemTime (MkSystemTime))
import qualified Data.Tuple.Extra as Tuple
import Foreign.Ptr (castPtr)
import qualified GI.Cairo (Context (..))
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render (runRender))
import Graphics.Rendering.Cairo.Types (Cairo (Cairo))

-- Constants
minimumScale :: Double
minimumScale = 0.1

panSpeed :: Double
panSpeed = 2000

baseFontSize :: Double
baseFontSize = 24

-- Types

-- | An Enum of mouse buttons, so order is important!
data MouseButton
  = LeftMouseButton
  | MiddleMouseButton
  | RightMouseButton
  | UnknownMouseButton
  deriving (Eq, Ord, Enum, Bounded, Show)

data KeyInput = KeyInput
  { _kiKeyString :: Text.Text,
    _kiKeyEvent :: KeyEvent
  }
  deriving (Show, Eq)

-- | What action should be taken when you press a key?
data KeyEvent
  = UndoKey
  | AbortKey
  | MouseTranslateKey
  | MoveUp
  | MoveLeft
  | MoveDown
  | MoveRight
  | OtherKey Text.Text
  deriving (Eq, Ord, Show)

-- | This is not an enmum so that new types of nodes can be created at
-- runtime. Everything of type (Double, Double) is either (width,
-- height) or (x, y).
data NodeType = NodeType
  { _ntName :: !String,
    _ntNumInitialPorts :: !Int,
    -- | Returns which port was clicked on. If no port was clicked it
    -- returns Nothing.
    _ntPortClicked ::
      !( (Double, Double) -> -- (x, y) in the node's coordinates.
         Element -> -- Which element was clicked
         Maybe Int
       ),
    _ntDraw ::
      !( Transform ->
         (Int, Element) ->
         Maybe Port -> -- Currently selected port
         Render ()
       ),
    -- | Given the port number, return the location of that port in
    -- the node's coordinates.
    _ntPortLocations :: !(Int -> (Double, Double)),
    -- | (x, y) position of the center of the handle.
    _ntHandleLocation :: !(Double, Double),
    -- ? Should the argument be Element instead?

    -- | Given the number of ports, return the size of the node.
    _ntSize :: !(Int -> (Double, Double))
  }

-- | A simple 2d transformation. See transform below for
-- details.
data Transform = Transform
  { -- | Controls scaling. This is a scale factor, so the size of
    -- visual elements are multiplied by this number. Thus a value of
    -- one is no scale, values greater than 1 scales in, and values
    -- between 0 and 1 scales out. Negative values result in undefined
    -- behavior, although it would be cool if negative values
    -- produced a flip across both the X and Y axes.
    _tScale :: !Double,
    -- | (x, y)
    _tTranslate :: !(Double, Double)
  }
  deriving (Show, Eq)

-- TODO Add quick check tests that transform and unTransform are inverses
transform :: Transform -> (Double, Double) -> (Double, Double)
transform (Transform scale (deltaX, deltaY)) (x, y) =
  ( x * scale + deltaX,
    y * scale + deltaY
  )

unTransform :: Transform -> (Double, Double) -> (Double, Double)
unTransform (Transform scale (deltaX, deltaY)) (transformedX, transformedY) =
  ( (transformedX - deltaX) / scale,
    (transformedY - deltaY) / scale
  )

windowToElementCoordinates :: AppState -> (Double, Double) -> (Double, Double)
windowToElementCoordinates state = unTransform (_asTransform state)

newtype ElemId = ElemId {_unElemId :: Int} deriving (Show, Eq, Ord, Num)

-- | A graphical element that can be clicked
data Element = Element
  { -- | (x, y) of top left corner. These values are in Element
    -- coordinates. Use the (transform _asTransform) function to
    -- convert these to window coordinates and (unTransform
    -- _asTransform) to convert window coordinates to _elPosition.
    _elPosition :: !(Double, Double),
    -- | Depth. Higher values are drawn on top
    -- _elZ is currently ignored
    _elZ :: !Int,
    _elType :: !NodeType,
    _elNumPorts :: !Int,
    _elPortText :: !(IntMap.IntMap Text.Text)
  }

-- | When the translation key is first pressed, these values contain
-- the mouse position and (_tTranslate . _asTransform) at that moment.
data Panning = Panning
  { _panMouse :: !(Double, Double),
    _panTranslation :: !(Double, Double)
  }
  deriving (Eq, Ord, Show)

-- | A specific port in a node.
data Port = Port
  { -- | The node this port is in.
    _pNode :: ElemId,
    -- | The port number of the port in the node.
    _pPort :: Int
  }
  deriving (Eq, Ord, Show)

data Inputs = Inputs
  { -- | Raw mouse x and y position in window coordinates.
    _inMouseXandY :: !(Double, Double),
    _inTime :: !SystemTime,
    _inPrevTime :: !SystemTime,
    _inEvents :: ![InputEvent],
    -- | If Something, then a translation is occuring.
    _inTranslation :: !(Maybe Panning),
    -- | All keys currently being pressed.
    _inPressedKeys :: !(Set.Set KeyEvent)
  }

-- TODO Consider extracting History and UndoPosition into their own "object".
data AppState = AppState
  { -- | This is a key for _asElements
    _asMovingNode :: !(Maybe ElemId),
    -- TODO _asEdges is a set, so consider using a set data structure here.

    -- | The connections between nodes. Each edge is a set of at least
    -- two ports that are connected.
    _asEdges ::
      !( Set.Set
           (Set.Set Port) -- Each Set of Ports is a single "edge".
       ),
    -- | Iff Just, an edge is currently being draw where the ElemId is
    -- one end of the edge.
    _asCurrentEdge :: !(Maybe Port),
    -- TODO current edge and current port have the same type. Can they
    -- be combined? Also, may want to edit the node type /
    -- handle. Should that be a "port" too?

    -- | Iff Just, then currently the text of this port is being edited.
    _asCurrentPort :: !(Maybe Port),
    _asElements :: !(IntMap.IntMap Element),
    -- | FPS rounded down to nearest hundred if over 200 fps.
    _asFPSr :: !Int,
    -- | A full history of the state of the app. Use addHistoryEvent
    -- to add new HistoryEvents, do not add events directly.
    _asHistory :: ![Undoable HistoryEvent],
    -- | A pointer into _asHistory. The undo command pops this
    -- stack, undos the HistoryEvent, and pushes the inverse of the
    -- popped HistoryEvent onto _asHistory.
    _asUndoPosition :: ![Undoable HistoryEvent],
    -- | The biggest ElemId used so far in the program. Not updated by undo.
    _asBiggestID :: !ElemId,
    -- | Controls scalaing (aka. zooming) and translation (aka. panning)
    _asTransform :: !Transform
  }

data InputEvent
  = -- | Which node was clicked and the relative click position within a node.
    ClickOnNode
      !MouseButton
      !ElemId
      !(Double, Double) -- relative mouse position
  | AddNode !(Double, Double) -- where to add the node
  | -- | Undo the last action.
    UndoEvent
  | -- | Abort the current command (like C-g in Emacs).
    AbortEvent
  | -- | The scale factor is multiplied by this number.
    ScaleAdjustEvent !Double

data Undoable a = Do !a | Undo !a

---------- NodeType instances ----------

---- Apply Node ----

handleWidth :: Double
handleWidth = 70

portWidth :: Double
portWidth = 100

applyNodeHeight :: Double
applyNodeHeight = 40

-- | Draw an apply node. This function's type will probably change
-- since some of this could be done in drawNode.
drawApply :: Transform -> (Int, Element) -> Maybe Port -> Render ()
drawApply transformation (elemId, Element {..}) selectedPort = do
  -- TODO See if there's a way to scale the entire Cairo drawing.
  let (x, y) = transform transformation _elPosition
      scale = _tScale transformation
      scaledPortWidth = scale * portWidth
      scaledHeight = scale * applyNodeHeight
      scaledHandleWidth = scale * handleWidth
      fontHeight = baseFontSize * scale
      drawPort portNum = do
        if
            | selectedPort == Just (Port {_pNode = ElemId elemId, _pPort = portNum}) ->
              Cairo.setSourceRGB 0 1 0
            | portNum == 0 -> Cairo.setSourceRGB 1 1 1
            | otherwise -> Cairo.setSourceRGB 1 0 0
        let portText = fromMaybe "" $ IntMap.lookup portNum _elPortText
        Cairo.rectangle
          (x + scaledHandleWidth + scaledPortWidth * fromIntegral portNum)
          y
          scaledPortWidth
          scaledHeight
        Cairo.moveTo xPos (y - 0.4 * fontHeight)
        Cairo.showText portText
        Cairo.stroke
        where
          xPos = x + scaledHandleWidth + scaledPortWidth * fromIntegral portNum

  Cairo.setSourceRGB 0.5 0.5 0.5
  Cairo.setLineWidth (3 * scale)
  -- Draw the handle
  Cairo.rectangle x y scaledHandleWidth scaledHeight
  Cairo.moveTo (x + 5 * scale) (y + scale * snd (_ntHandleLocation _elType))
  -- Cairo.showText (show elemId <> " " <> _ntName _elType)
  Cairo.stroke
  Cairo.setSourceRGB 1 0 0
  traverse_ drawPort [0 .. (_elNumPorts -1)]
  Cairo.stroke

applyPortClicked :: (Double, Double) -> Element -> Maybe Int
applyPortClicked (x, _) Element {_elNumPorts} =
  let rectClicked = floor ((x - handleWidth) / portWidth)
   in if x <= handleWidth
        then Nothing
        else Just rectClicked

applyPortLocations :: Int -> (Double, Double)
applyPortLocations port =
  ( handleWidth + (portWidth / 2) + (fromIntegral port * portWidth),
    applyNodeHeight / 2
  )

applySize :: Int -> (Double, Double)
applySize numPorts =
  ( handleWidth + portWidth * fromIntegral numPorts,
    applyNodeHeight
  )

apply :: NodeType
apply =
  NodeType
    { _ntName = "apply",
      _ntNumInitialPorts = 3,
      _ntPortClicked = applyPortClicked,
      _ntDraw = drawApply,
      _ntPortLocations = applyPortLocations,
      _ntHandleLocation = (handleWidth / 2, applyNodeHeight / 2),
      _ntSize = applySize
    }

---- TextNode ----

-------- Types ----------

-- | Flip a Do to an Undo, and an Undo to a Do.
invertUndoable :: Undoable a -> Undoable a
invertUndoable undoable = case undoable of
  Do a -> Undo a
  Undo a -> Do a

-- | Records actions so that they can be undone.
data HistoryEvent
  = MovedNode -- TODO Record which node, and where the node was moved
  -- from (and to).
  | AddedNode ElemId (Double, Double) -- Id of node and position
  deriving (Show, Eq)

emptyAppState :: AppState
emptyAppState =
  AppState
    { _asMovingNode = Nothing,
      _asEdges = mempty,
      _asCurrentEdge = Nothing,
      _asCurrentPort = Nothing,
      _asElements = mempty,
      _asFPSr = 0,
      _asHistory = [],
      _asUndoPosition = [],
      _asBiggestID = 0,
      _asTransform = Transform 1 (0, 0)
    }

emptyInputs :: Inputs
emptyInputs =
  Inputs
    { _inMouseXandY = (0, 0),
      _inTime = MkSystemTime 0 0,
      _inPrevTime = MkSystemTime 0 0,
      _inEvents = mempty,
      _inTranslation = Nothing,
      _inPressedKeys = mempty
    }

-- | Add a new HistoryEvent and reset _asUndoPosition.
addHistoryEvent :: HistoryEvent -> AppState -> AppState
addHistoryEvent event state@AppState {_asHistory, _asUndoPosition} =
  let updatedHistory = Do event : _asHistory
   in state
        { _asHistory = updatedHistory,
          _asUndoPosition = updatedHistory
        }

-- | Add an event to the event queue in Inputs.
addEvent :: InputEvent -> Inputs -> Inputs
addEvent event inputs@Inputs {_inEvents} = inputs {_inEvents = event : _inEvents}

-- | Uses the argument function to combine the tuples.
elementwiseOp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
elementwiseOp f (x0, x1) (y0, y1) =
  (f x0 y0, f x1 y1)

renderCairo :: Coercible a GI.Cairo.Context => a -> Render c -> IO c
renderCairo c r = withManagedPtr c $ \pointer ->
  runReaderT (runRender r) (Cairo (castPtr pointer))

drawLine :: Transform -> (Double, Double) -> (Double, Double) -> Render ()
drawLine Transform {_tScale} (fromX, fromY) (toX, toY) = do
  Cairo.setSourceRGB 0 1 0
  Cairo.setLineWidth (5 * _tScale)

  Cairo.moveTo fromX fromY
  Cairo.lineTo toX toY
  Cairo.stroke

_drawCircle :: (Double, Double) -> Render ()
_drawCircle (x, y) = do
  --   setSourceRGB 1 0 0
  Cairo.setLineWidth 1
  -- moveTo x y
  let radius = 20
  let tau = 2 * pi
  Cairo.arc x y radius 0 tau
  Cairo.stroke

drawNode :: AppState -> (Int, Element) -> Render ()
drawNode AppState {_asCurrentPort, _asTransform} (elemId, element) =
  _ntDraw (_elType element) _asTransform (elemId, element) _asCurrentPort

-- TODO This name should indicate that it's adding in the location of
-- the element.
portLocation :: Int -> Element -> (Double, Double)
portLocation port element =
  elementwiseOp
    (+)
    (_elPosition element)
    (_ntPortLocations (_elType element) port)

drawCurrentEdge :: (Double, Double) -> AppState -> Render ()
drawCurrentEdge mousePosition AppState {_asCurrentEdge, _asElements, _asTransform} =
  case _asCurrentEdge of
    Nothing -> pure ()
    Just port -> case IntMap.lookup (_unElemId $ _pNode port) _asElements of
      Nothing -> pure ()
      Just element ->
        drawLine
          _asTransform
          (transform _asTransform (portLocation (_pPort port) element))
          mousePosition

drawEdges :: AppState -> Render ()
drawEdges AppState {_asEdges, _asElements, _asTransform} =
  traverse_
    drawEdge
    (Set.toList _asEdges >>= allPairs)
  where
    -- TODO Replace with a minimum spanning tree.
    -- For each edge, generate all pairs of ports
    allPairs :: Set.Set Port -> [(Port, Port)]
    allPairs portSet = do
      let ports = Set.toList portSet
      a <- ports
      let portsMinusA = Set.toList (Set.delete a portSet)
      b <- portsMinusA
      [(a, b)]

    drawEdge :: (Port, Port) -> Render ()
    drawEdge (from, to) = case (lookupFrom, lookupTo) of
      (Just fromElem, Just toElem) ->
        drawLine
          _asTransform
          (transform _asTransform (portLocation (_pPort from) fromElem))
          (transform _asTransform (portLocation (_pPort to) toElem))
      _ -> pure ()
      where
        lookupFrom = IntMap.lookup (_unElemId $ _pNode from) _asElements
        lookupTo = IntMap.lookup (_unElemId $ _pNode to) _asElements

updateBackground :: p -> IORef Inputs -> IORef AppState -> Render ()
updateBackground _canvas inputsRef stateRef = do
  -- width  <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedWidth  canvas)
  --             :: Render Double)
  -- height <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedHeight canvas)
  --             :: Render Double)

  -- TODO This should be moved into the setup phase
  Cairo.setSourceRGB 0 0 0
  Cairo.paint
  Cairo.setFontSize baseFontSize

  state <- Cairo.liftIO $ readIORef stateRef
  inputs <- Cairo.liftIO $ readIORef inputsRef
  let fps = _asFPSr state
  if fps >= 120
    then Cairo.setSourceRGB 1 1 1
    else Cairo.setSourceRGB 1 0 0
  Cairo.moveTo 10 baseFontSize
  Cairo.showText ("fps=" <> show fps)
  Cairo.setFontSize (baseFontSize * _tScale (_asTransform state))
  drawCurrentEdge (_inMouseXandY inputs) state
  drawEdges state
  traverse_
    (drawNode state)
    (IntMap.toList (_asElements state))

findElementByPosition ::
  IntMap.IntMap Element -> (Double, Double) -> Maybe (Int, Element)
findElementByPosition elements (mouseX, mouseY) =
  let mouseInElement (_elementId, Element {_elPosition, _elType, _elNumPorts}) =
        let (x, y) = _elPosition
            (width, height) = _ntSize _elType _elNumPorts
         in mouseX >= x && mouseX <= (x + width)
              && mouseY >= y
              && mouseY <= (y + height)
   in find mouseInElement (IntMap.toList elements)

-- | Time between steps in seconds.
calcFrameTime :: Inputs -> Double
calcFrameTime Inputs {_inTime, _inPrevTime} =
  frameTime
  where
    nanoToSecond nanoSecond = fromIntegral nanoSecond / (10 ^ (9 :: Int))
    frameTime =
      fromIntegral secondsDiff + nanosecondDiff
    (MkSystemTime seconds nanoseconds) = _inTime
    (MkSystemTime oldSeconds oldNanoseconds) = _inPrevTime
    secondsDiff = seconds - oldSeconds
    -- nanoseconds in MkSystemTime are unsigned, so they can not be
    -- directly subtracted.
    nanosecondDiff = nanoToSecond nanoseconds - nanoToSecond oldNanoseconds

getFps :: Inputs -> Int
getFps inputs =
  let fps :: Int
      fps = round $ 1 / calcFrameTime inputs
   in if fps >= 200
        then div fps 100 * (100 :: Int)
        else fps

clickOnNode ::
  MouseButton ->
  ElemId ->
  (Double, Double) -> -- Click position where (0,0) is top left of element
  AppState ->
  AppState
clickOnNode mouseButton = case mouseButton of
  LeftMouseButton -> clickOnNodePrimaryAction
  RightMouseButton -> clickOnNodeSecondaryAction
  _ -> \_ _ -> id

clickOnNodePrimaryAction ::
  ElemId ->
  (Double, Double) -> -- Click position where (0,0) is top left of element
  AppState ->
  AppState
clickOnNodePrimaryAction
  elemId
  relativePosition
  oldState@AppState {_asMovingNode, _asHistory, _asElements, _asCurrentEdge, _asEdges} =
    let portClicked = case IntMap.lookup (_unElemId elemId) _asElements of
          Nothing -> Nothing
          Just element ->
            _ntPortClicked (_elType element) relativePosition element
     in case _asMovingNode of
          Nothing -> case portClicked of
            Nothing -> oldState {_asMovingNode = Just elemId}
            Just port -> case _asCurrentEdge of
              Nothing -> oldState {_asCurrentEdge = Just $ Port {_pNode = elemId, _pPort = port}}
              Just edgePort ->
                oldState
                  { _asEdges =
                      -- TODO May want to add one of the ports to the other's existing edge.
                      Set.insert
                        ( Set.fromList
                            [ edgePort,
                              Port {_pNode = elemId, _pPort = port}
                            ]
                        )
                        _asEdges,
                    _asCurrentEdge = Nothing
                  }
          Just _ ->
            addHistoryEvent MovedNode $
              oldState {_asMovingNode = Nothing}

clickOnNodeSecondaryAction ::
  ElemId ->
  (Double, Double) -> -- Click position where (0,0) is top left of element
  AppState ->
  AppState
clickOnNodeSecondaryAction
  elemId
  relativePosition
  oldState@AppState {_asMovingNode, _asHistory, _asElements, _asCurrentPort, _asEdges} =
    let portClicked = case IntMap.lookup (_unElemId elemId) _asElements of
          Nothing -> Nothing
          Just element ->
            _ntPortClicked (_elType element) relativePosition element
     in case (portClicked, _asCurrentPort) of
          (Just port, _) -> oldState {_asCurrentPort = Just $ Port {_pNode = elemId, _pPort = port}}
          _ -> oldState {_asCurrentPort = Nothing}

-- | Add a node to the canvas at the given position in Element
-- coordinates with a known ID.
addNodeWithId :: ElemId -> (Double, Double) -> AppState -> AppState
addNodeWithId
  nodeId
  addPosition
  state@AppState {_asElements, _asHistory, _asBiggestID} =
    let applyNode =
          Element
            { _elPosition = addPosition,
              _elZ = 0,
              _elType = apply,
              _elNumPorts = _ntNumInitialPorts apply,
              _elPortText = mempty
            }
        newElements =
          IntMap.insert (_unElemId nodeId) applyNode _asElements
     in addHistoryEvent (AddedNode nodeId addPosition) $
          state
            { _asElements = newElements,
              _asBiggestID = max _asBiggestID nodeId
            }

-- | Add a node to the canvas at the given position.
addNode :: (Double, Double) -> AppState -> AppState
addNode addPosition state@AppState {_asBiggestID} =
  addNodeWithId (1 + _asBiggestID) addPosition state

removeNode :: ElemId -> AppState -> AppState
removeNode nodeId oldState@AppState {_asElements} =
  oldState {_asElements = IntMap.delete (_unElemId nodeId) _asElements}

undo :: AppState -> AppState
undo oldState@AppState {_asHistory, _asUndoPosition} = newState
  where
    newState = case _asUndoPosition of
      [] -> oldState
      historyEvent : restOfHistory ->
        undidState
          { _asHistory = invertUndoable historyEvent : _asHistory,
            _asUndoPosition = restOfHistory
          }
        where
          undidState = case historyEvent of
            Do MovedNode -> oldState -- TODO Implement undo move node.
            Do (AddedNode nodeId _) -> removeNode nodeId oldState
            Undo (AddedNode nodeId position) ->
              addNodeWithId nodeId position oldState
            Undo MovedNode -> oldState -- TODO Implement undo Undo move.

-- | Abort the current action. This includes resetting the _asUndoPosition.
abort :: AppState -> AppState
abort state@AppState {_asHistory, _asUndoPosition} =
  state {_asUndoPosition = _asHistory}

-- | Adjust the scale factor.
adjustScale :: (Double, Double) -> Double -> AppState -> AppState
adjustScale mousePosition scaleAdjustment state@AppState {_asTransform} =
  state
    { _asTransform =
        _asTransform
          { _tScale = newScale,
            _tTranslate = newTranslate
          }
    }
  where
    oldScale = _tScale _asTransform
    adjustedScale = oldScale * scaleAdjustment
    newScale =
      if adjustedScale > minimumScale
        then adjustedScale
        else oldScale
    -- When zooming, elements that are at the mouse position should
    -- stay at the mouse position under the new transform. Elements at
    -- the mouse position are at
    -- unTransform _asTransform mousePosition
    -- So
    -- mousePosition =
    --   transform newTransform (unTransform _asTransform mousePosision)
    -- Which when solved for the new translation produces the code below.
    unTransformedMousePosition = unTransform _asTransform mousePosition
    newTranslate =
      elementwiseOp
        (-)
        mousePosition
        ( Tuple.both
            (* newScale)
            unTransformedMousePosition
        )

processInput :: Inputs -> InputEvent -> AppState -> AppState
processInput Inputs {_inMouseXandY} inputEvent oldState =
  case inputEvent of
    ClickOnNode mouseButton elemId relativePosition -> clickOnNode mouseButton elemId relativePosition oldState
    AddNode addPosition -> addNode addPosition oldState
    UndoEvent -> undo oldState
    AbortEvent -> abort oldState
    ScaleAdjustEvent scaleAdjustment -> adjustScale _inMouseXandY scaleAdjustment oldState

processInputs :: Inputs -> AppState -> AppState
processInputs
  inputs@Inputs {_inEvents}
  oldState@AppState {_asElements, _asMovingNode} =
    let compose = foldr (.) id
     in compose (fmap (processInput inputs) _inEvents) oldState

-- The amount by which to adjust the translation when the panning keys
-- are pressed.
keyPanAdjustment :: Double -> Set.Set KeyEvent -> (Double, Double)
keyPanAdjustment frameTime pressedKeys = (xAdjust, yAdjust)
  where
    moveAmount = frameTime * panSpeed
    moveLeft = if Set.member MoveLeft pressedKeys then moveAmount else 0
    moveRight = if Set.member MoveRight pressedKeys then (- moveAmount) else 0
    moveUp = if Set.member MoveUp pressedKeys then moveAmount else 0
    moveDown = if Set.member MoveDown pressedKeys then (- moveAmount) else 0
    xAdjust = moveLeft + moveRight
    yAdjust = moveUp + moveDown

-- | Update the state based on the inputs and the old state.
updateState :: Inputs -> AppState -> AppState
updateState
  inputs@Inputs {_inMouseXandY, _inEvents, _inTranslation, _inPressedKeys}
  oldState@AppState {_asElements, _asMovingNode, _asTransform} =
    let -- Move the asMovingNode to MouseXandY
        newElements = case _asMovingNode of
          Nothing -> _asElements
          Just nodeId ->
            IntMap.adjust
              ( \oldNode@Element {_elPosition, _elType} ->
                  let newPosition =
                        elementwiseOp
                          (-)
                          (unTransform _asTransform _inMouseXandY)
                          (_ntHandleLocation _elType)
                   in oldNode {_elPosition = newPosition}
              )
              (_unElemId nodeId)
              _asElements
        mousePanTranslate =
          case _inTranslation of
            Nothing -> _tTranslate _asTransform
            Just (Panning initialMousePosition initialTranslation) ->
              elementwiseOp
                (+)
                initialTranslation
                (elementwiseOp (-) _inMouseXandY initialMousePosition)

        newTranslate =
          elementwiseOp
            (+)
            mousePanTranslate
            ( keyPanAdjustment
                (calcFrameTime inputs)
                _inPressedKeys
            )
     in oldState
          { _asElements = newElements,
            _asFPSr = getFps inputs,
            _asTransform = _asTransform {_tTranslate = newTranslate}
          }

---------- Input Callbacks ---------------------------------
leftClickAction ::
  IORef Inputs ->
  IORef AppState ->
  (Double, Double) ->
  IO ()
leftClickAction inputsRef stateRef mousePosition =
  do
    state <- readIORef stateRef
    -- print (_asEdges state)
    let mElem = findElementByPosition (_asElements state) mousePosition

        addClickEvent inputs@Inputs {_inEvents} =
          case mElem of
            Nothing -> inputs
            Just (elemId, element) ->
              addEvent
                ( ClickOnNode
                    LeftMouseButton
                    (ElemId elemId)
                    (elementwiseOp (-) mousePosition (_elPosition element))
                )
                inputs
    writeIORef stateRef (state {_asCurrentPort = Nothing})
    modifyIORef'
      inputsRef
      addClickEvent

-- TODO Refactor with leftClickAction. Passing the MouseButton to
-- ClickOnNode feels wrong since actions should be separate from
-- inputs. Perhaps PrimaryAction and SecondaryAction instead?
rightClickAction ::
  IORef Inputs ->
  IORef AppState ->
  (Double, Double) ->
  IO ()
rightClickAction inputsRef stateRef mousePosition =
  do
    state <- readIORef stateRef
    let mElem = findElementByPosition (_asElements state) mousePosition
        addClickEvent inputs@Inputs {_inEvents} =
          case mElem of
            Nothing ->
              addEvent (AddNode mousePosition) inputs
            Just (elemId, element) ->
              addEvent
                ( ClickOnNode -- TODO Event should be move node or
                -- something like that. Selecting a port should be
                -- instant, not an event.
                    RightMouseButton
                    (ElemId elemId)
                    (elementwiseOp (-) mousePosition (_elPosition element))
                )
                inputs
    when
      (isNothing mElem)
      (writeIORef stateRef (state {_asCurrentPort = Nothing}))

    modifyIORef'
      inputsRef
      addClickEvent
    pure ()

backgroundPress ::
  IORef Inputs ->
  IORef AppState ->
  MouseButton ->
  (Double, Double) ->
  IO ()
backgroundPress inputsRef stateRef mouseButton rawMousePosition = do
  putStrLn ("Background pressed by " <> show mouseButton)
  state <- readIORef stateRef
  let mousePosition = windowToElementCoordinates state rawMousePosition
  case mouseButton of
    RightMouseButton -> rightClickAction inputsRef stateRef mousePosition
    LeftMouseButton ->
      leftClickAction inputsRef stateRef mousePosition
    _ -> mempty

addUndoInputAction :: IORef Inputs -> IO ()
addUndoInputAction inputsRef = do
  putStrLn "Undo"
  modifyIORef' inputsRef (addEvent UndoEvent)
  pure ()

addAbortAction :: IORef Inputs -> IO ()
addAbortAction inputsRef = do
  putStrLn "Abort"
  modifyIORef' inputsRef (addEvent AbortEvent)
  pure ()

editPortText :: Port -> Text.Text -> AppState -> AppState
editPortText Port {_pNode, _pPort} newText oldState@AppState {_asElements} =
  oldState {_asElements = IntMap.adjust modifyElem (_unElemId _pNode) _asElements}
  where
    modifyElem :: Element -> Element
    modifyElem el@Element {_elPortText} =
      el {_elPortText = IntMap.alter changeText _pPort _elPortText}

    changeText :: (Maybe Text.Text -> Maybe Text.Text)
    changeText = case newText of
      -- "\b" is backspace
      "\b" ->
        \mText -> do
          -- Maybe monad
          text <- mText
          (inits, _) <- Text.unsnoc text
          pure inits
      _ -> Just . maybe newText (<> newText)

keyPress :: IORef Inputs -> IORef AppState -> KeyInput -> IO ()
keyPress inputsRef stateRef keyInput = do
  -- print keyInput
  let keyEvent = _kiKeyEvent keyInput
  state <- readIORef stateRef
  preKeyPressedInputs <- readIORef inputsRef
  case _asCurrentPort state of
    Just port -> modifyIORef' stateRef (editPortText port (_kiKeyString keyInput))
    Nothing -> do
      let inputs =
            preKeyPressedInputs
              { _inPressedKeys =
                  Set.insert keyEvent (_inPressedKeys preKeyPressedInputs)
              }
      writeIORef inputsRef inputs
      -- print (_inPressedKeys inputs)
      case keyEvent of
        UndoKey -> addUndoInputAction inputsRef
        AbortKey -> addAbortAction inputsRef
        MouseTranslateKey ->
          if
              | isNothing (_inTranslation inputs) ->
                writeIORef
                  inputsRef
                  ( inputs
                      { _inTranslation =
                          Just
                            ( Panning
                                (_inMouseXandY inputs)
                                (_tTranslate (_asTransform state))
                            )
                      }
                  )
                  >> putStrLn "translate key pressed"
              | otherwise -> pure ()
        OtherKey keyStr -> print keyStr
        _ -> pure ()

-- pure
-- ()

keyRelease :: IORef Inputs -> KeyInput -> IO ()
keyRelease inputsRef keyInput = do
  let keyEvent = _kiKeyEvent keyInput
  modifyIORef'
    inputsRef
    ( \inputs ->
        inputs
          { _inPressedKeys = Set.delete keyEvent (_inPressedKeys inputs)
          }
    )
  case keyEvent of
    MouseTranslateKey ->
      modifyIORef'
        inputsRef
        ( \inputs ->
            inputs
              { _inTranslation = Nothing
              }
        )
        >> putStrLn "translate key released"
    _ -> pure ()

addScaleAdjustAction ::
  -- |  Amount by which to change the scale factor
  Double ->
  IORef Inputs ->
  IO ()
addScaleAdjustAction scaleDelta inputsRef = do
  -- putStrLn ("Adjusting scale by " <> show scaleDelta)
  modifyIORef' inputsRef (addEvent (ScaleAdjustEvent scaleDelta))
  pure ()

scroll :: IORef Inputs -> Double -> IO ()
scroll inputsRef deltaY =
  -- scale in (zooming in) is negative (usually -1), scale out
  -- (zooming out) is positive (usually 1)
  if deltaY /= 0.0
    then addScaleAdjustAction (1 - (0.2 * deltaY)) inputsRef
    else pure ()

-- | Called by GtkGui.timeoutCallback
step ::
  IORef Inputs ->
  IORef AppState ->
  SystemTime ->
  (Double, Double) ->
  IO a ->
  IO a
step inputsRef stateRef newTime (mouseX, mouseY) drawCommand = do
  modifyIORef'
    inputsRef
    ( \inputs@Inputs {_inTime} ->
        inputs
          { _inMouseXandY = (mouseX, mouseY),
            _inTime = newTime,
            _inPrevTime = _inTime
          }
    )
  inputs <- readIORef inputsRef
  modifyIORef' stateRef (updateState inputs . processInputs inputs)
  modifyIORef' inputsRef (\i -> i {_inEvents = []}) -- Clear the event queue.
  -- state <- readIORef stateRef
  -- print (_asCurrentPort state)
  drawCommand
