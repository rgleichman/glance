-- This file is formatted with Ormolu
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, mouseButtonNum) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Coerce (Coercible)
import Data.Foldable (traverse_)
import Data.GI.Base (AttrOp ((:=)), new, withManagedPtr)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
-- import qualified GI.GdkPixbuf as GP
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)
import qualified Data.Tuple.Extra as Tuple
import Foreign.Ptr (castPtr)
import GHC.Word (Word32)
import qualified GI.Cairo (Context (..))
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render (runRender))
import Graphics.Rendering.Cairo.Types (Cairo (Cairo))

-- Constants
minimumScale :: Double
minimumScale = 0.1

nodeSize :: (Double, Double)
nodeSize = (100, 40)

-- Types

-- | A simple 2d transformation. See transform below for
-- details.
data Transform = Transform
  { -- | Controls scaleing. This is a scale factor, so the size of
    -- vizual elements are multiplied by this number. Thus a value of
    -- one is no scale, values greater than 1 scales in, and values
    -- between 0 and 1 scales out. Negative values result in undefined
    -- behaviour, although it would be cool if negative values
    -- produced a flip across both the X and Y axes.
    _tScale :: !Double,
    -- (x, y)
    _tTranslate :: !(Double, Double)
  }

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

-- | An Enum of mouse buttons, so order is important!
data MouseButton
  = LeftMouseButton
  | MiddleMouseButton
  | RightMouseButton
  | UnknownMouseButton
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | A mapping between mouse button names and the GTK
-- mouse button numbers via Enum, so order is important!
mouseButtonNum :: MouseButton -> Word32
mouseButtonNum = fromIntegral . (+ 1) . fromEnum

-- | Convert a GDK mouse button number to a MouseButton. If the button
-- is not recognized, return UnknownMouseButton.
toMouseButton :: Word32 -> MouseButton
toMouseButton gtkMouseButton =
  if enumNum < fromEnum (minBound :: MouseButton)
    || enumNum > fromEnum (maxBound :: MouseButton)
    then UnknownMouseButton
    else --  toEnum will cause an exception if enumNum is out bounds.
      toEnum enumNum
  where
    enumNum = fromIntegral (gtkMouseButton - 1)

newtype ElemId = ElemId {_unElemId :: Int} deriving (Show, Eq, Ord, Num)

-- | A graphical element that can be clicked
data Element = Element
  { -- | (x, y) of top left corner. These values are in Element
    -- coordinates. Use the (transform _asTransform) function to
    -- convert these to window coordinates and (unTransform
    -- _asTransform) to convert window coordinates to _elPosition.
    _elPosition :: !(Double, Double),
    -- | (width, height)
    _elSize :: !(Double, Double),
    -- | Depth. Higher values are drawn on top
    -- _elZ is currently ignored
    _elZ :: Int
  }

data Inputs = Inputs
  { _inMouseXandY :: !(Double, Double),
    _inTime :: SystemTime,
    _inPrevTime :: SystemTime,
    _inEvents :: [InputEvent]
  }

-- TODO Consider extracting History and UndoPosition into their own "object".
data AppState = AppState
  { -- | This is a key for _asElements
    _asMovingNode :: Maybe ElemId,
    _asEdges :: [(Element, Element)],
    _asElements :: IntMap.IntMap Element,
    -- | FPS rounded down to nearest hundred if over 200 fps.
    _asFPSr :: Double,
    -- | A full history of the state of the app. Use addHistoryEvent
    -- to add new HistoryEvents, do not add events directly.
    _asHistory :: [Undoable HistoryEvent],
    -- | A pointer into _asHistory. The undo command pops this
    -- stack, undos the HistoryEvent, and pushes the inverse of the
    -- popped HistoryEvent onto _asHistory.
    _asUndoPosition :: [Undoable HistoryEvent],
    -- | The biggest ElemId used so far in the program. Not updated by undo.
    _asBiggestID :: ElemId,
    -- | Controls scalaing (aka. zooming) and translation (aka. panning)
    _asTransform :: Transform
  }

data InputEvent
  = -- | Which node was clicked and the relative click position within a node.
    ClickOnNode
      ElemId
      (Double, Double) -- relative mouse position
  | AddNode (Double, Double) -- where to add the node
  | -- | Undo the last action.
    UndoEvent
  | -- | Abort the current command (like C-g in Emacs).
    AbortEvent
  | -- | The scale factor is multiplied by this number.
    ScaleAdjustEvent Double

data Undoable a = Do a | Undo a

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
      _asEdges = [],
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
      _inEvents = mempty
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

getXandY ::
  MonadIO f =>
  Gdk.EventButton ->
  f (Double, Double)
getXandY event =
  (\x y -> (x, y)) <$> Gdk.getEventButtonX event <*> Gdk.getEventButtonY event

_drawLine :: (Double, Double) -> (Double, Double) -> Render ()
_drawLine (fromX, fromY) (toX, toY) = do
  Cairo.setSourceRGB 0 1 0
  Cairo.setLineWidth 5

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

drawNode :: Transform -> (Int, Element) -> Render ()
drawNode transformation (elemId, Element {..}) = do
  let (x, y) = transform transformation _elPosition
      scale = _tScale transformation
      (width, height) = Tuple.both (* scale) _elSize
      halfWidth = width / 2

  Cairo.setSourceRGB 1 0 0
  Cairo.setLineWidth (3 * scale)
  Cairo.rectangle x y halfWidth height
  Cairo.showText (show elemId)
  Cairo.stroke
  Cairo.setSourceRGB 0 1 0
  Cairo.rectangle (x + halfWidth) y halfWidth height
  Cairo.stroke

updateBackground :: p -> IORef AppState -> Render ()
updateBackground _canvas stateRef = do
  -- width  <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedWidth  canvas)
  --             :: Render Double)
  -- height <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedHeight canvas)
  --             :: Render Double)

  -- TODO This should be moved into the setup phase
  Cairo.setSourceRGB 0 0 0
  Cairo.paint

  stateVal <- Cairo.liftIO $ readIORef stateRef
  Cairo.setSourceRGB 1 1 1
  Cairo.moveTo 10 10
  Cairo.showText ("fps=" <> show (_asFPSr stateVal))
  Cairo.setSourceRGB 1 0 0
  traverse_ (drawNode (_asTransform stateVal)) (IntMap.toList (_asElements stateVal))

findElementByPosition ::
  IntMap.IntMap Element -> (Double, Double) -> Maybe (Int, Element)
findElementByPosition elements (mouseX, mouseY) =
  let mouseInElement (_elementId, Element {_elPosition, _elSize}) =
        let (x, y) = _elPosition
            (width, height) = _elSize
         in mouseX >= x && mouseX <= (x + width)
              && mouseY >= y
              && mouseY <= (y + height)
   in find mouseInElement (IntMap.toList elements)

getFps :: Inputs -> Double
getFps inputs =
  let (MkSystemTime seconds nanoseconds) = _inTime inputs
      (MkSystemTime oldSeconds oldNanoseconds) = _inPrevTime inputs
      secondsDiff = seconds - oldSeconds
      nanosecondDiff = nanoseconds - oldNanoseconds
      fps =
        if secondsDiff == 0
          then fromIntegral (div (10 ^ (9 :: Int)) nanosecondDiff)
          else 1 / fromIntegral secondsDiff
   in if fps >= 200
        then fromIntegral $ div (truncate fps) 100 * (100 :: Int)
        else fps

clickOnNode :: ElemId -> AppState -> AppState
clickOnNode elemId oldState@AppState {_asMovingNode, _asHistory} =
  case _asMovingNode of
    Nothing -> oldState {_asMovingNode = Just elemId}
    Just _ ->
      addHistoryEvent MovedNode $
        oldState {_asMovingNode = Nothing}

-- | Add a node to the canvas at the given position in Element
-- coordinates with a known ID.
addNodeWithId :: ElemId -> (Double, Double) -> AppState -> AppState
addNodeWithId
  nodeId
  addPosition
  state@AppState {_asElements, _asHistory, _asBiggestID} =
    let newNode =
          Element
            { _elPosition = addPosition,
              _elSize = nodeSize,
              _elZ = 0
            }
        newElements =
          IntMap.insert (_unElemId nodeId) newNode _asElements
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
adjustScale :: Double -> AppState -> AppState
adjustScale scaleAdjustment state@AppState {_asTransform} =
  state
    { _asTransform = _asTransform {_tScale = newScale}
    }
  where
    oldScale = _tScale _asTransform
    adjustedScale = oldScale * scaleAdjustment
    newScale =
      if adjustedScale > minimumScale
        then adjustedScale
        else oldScale

processInput :: InputEvent -> AppState -> AppState
processInput inputEvent oldState =
  case inputEvent of
    ClickOnNode elemId _relativePosition -> clickOnNode elemId oldState
    AddNode addPosition -> addNode addPosition oldState
    UndoEvent -> undo oldState
    AbortEvent -> abort oldState
    ScaleAdjustEvent scaleAdjustment -> adjustScale scaleAdjustment oldState

processInputs :: Inputs -> AppState -> AppState
processInputs
  Inputs {_inEvents}
  oldState@AppState {_asElements, _asMovingNode} =
    let compose = foldr (.) id
     in compose (fmap processInput _inEvents) oldState

-- | Update the state based on the inputs and the old state.
updateState :: Inputs -> AppState -> AppState
updateState
  inputs@Inputs {_inMouseXandY, _inEvents}
  oldState@AppState {_asElements, _asMovingNode, _asTransform} =
    let -- Move the asMovingNode to MouseXandY
        newElements = case _asMovingNode of
          Nothing -> _asElements
          Just nodeId ->
            IntMap.adjust
              ( \oldNode@Element {_elPosition, _elSize} ->
                  let newPosition =
                        elementwiseOp
                          (-)
                          (unTransform _asTransform _inMouseXandY)
                          (Tuple.both (* 0.5) _elSize)
                   in oldNode {_elPosition = newPosition}
              )
              (_unElemId nodeId)
              _asElements
     in oldState
          { _asElements = newElements,
            _asFPSr = getFps inputs
          }

timeoutCallback ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.Window ->
  Gdk.Device ->
  Gtk.DrawingArea ->
  IO Bool
timeoutCallback inputsRef stateRef gdkWindow device backgroundArea = do
  newTime <- getSystemTime
  gdkDevicePosition <- Gdk.windowGetDevicePositionDouble gdkWindow device
  let (_, mouseX, mouseY, _) = gdkDevicePosition

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
  Gtk.widgetQueueDraw backgroundArea
  -- Use Gdk.EVENT_PROPAGATE to continue propagating the event.
  pure Gdk.EVENT_STOP

windowToElementCoordinates :: AppState -> (Double, Double) -> (Double, Double)
windowToElementCoordinates state = unTransform (_asTransform state)

leftClickAction ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  IO ()
leftClickAction inputsRef stateRef eventButton =
  do
    state <- readIORef stateRef
    mousePosition <- windowToElementCoordinates state <$> getXandY eventButton

    let mElem = findElementByPosition (_asElements state) mousePosition

        addClickEvent inputs@Inputs {_inEvents} =
          case mElem of
            Nothing -> inputs
            Just (elemId, element) ->
              addEvent
                ( ClickOnNode
                    (ElemId elemId)
                    (elementwiseOp (-) mousePosition (_elPosition element))
                )
                inputs
    modifyIORef'
      inputsRef
      addClickEvent

rightClickAction ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  IO ()
rightClickAction inputsRef stateRef eventButton =
  do
    state <- readIORef stateRef
    clickPosition <- windowToElementCoordinates state <$> getXandY eventButton

    modifyIORef'
      inputsRef
      (addEvent (AddNode clickPosition))
    pure ()

backgroundPress ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  IO Bool
backgroundPress inputsRef stateRef eventButton = do
  mouseButton <- toMouseButton <$> Gdk.getEventButtonButton eventButton
  putStrLn ("Background pressed by " <> show mouseButton)
  case mouseButton of
    RightMouseButton -> rightClickAction inputsRef stateRef eventButton
    LeftMouseButton ->
      leftClickAction inputsRef stateRef eventButton
    _ -> mempty
  pure Gdk.EVENT_STOP

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

addScaleAdjustAction ::
  -- |  Amount by which to change the scale factor
  Double ->
  IORef Inputs ->
  IO ()
addScaleAdjustAction scaleDelta inputsRef = do
  putStrLn ("Adjusting scale by " <> show scaleDelta)
  modifyIORef' inputsRef (addEvent (ScaleAdjustEvent scaleDelta))
  pure ()

keyPress :: IORef Inputs -> Gdk.EventKey -> IO Bool
keyPress inputsRef eventKey = do
  -- TODO May want to check that ctrl is pressed by checking that
  -- getEventKeyState is ModifierTypeControlMask. May also want to use
  -- Gdk.KEY_?.
  key <- Gdk.getEventKeyString eventKey
  case key of
    Just "\SUB" -> addUndoInputAction inputsRef -- ctrl-z pressed
    Just "\a" -> addAbortAction inputsRef -- ctrl-g
    -- _ -> print key
    _ -> pure ()
  pure Gdk.EVENT_STOP

scroll :: IORef Inputs -> Gdk.EventScroll -> IO Bool
scroll inputsRef scrollEvent = do
  -- scale in (zooming in) is negative (usually -1), scale out
  -- (zooming out) is positive (usually 1)
  deltaY <- Gdk.getEventScrollDeltaY scrollEvent
  print ("scroll y: " <> show deltaY)
  if deltaY /= 0.0
    then addScaleAdjustAction (1 - (0.2 * deltaY)) inputsRef
    else pure ()
  pure Gdk.EVENT_STOP

startApp :: Gtk.Application -> IO ()
startApp app = do
  stateRef <- newIORef emptyAppState
  inputsRef <- newIORef emptyInputs
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "Glance",
        #defaultWidth := 500,
        #defaultHeight := 500,
        #borderWidth := 0
      ]
  backgroundArea <- new Gtk.DrawingArea []
  Gtk.widgetAddEvents window [Gdk.EventMaskKeyPressMask]
  Gtk.widgetAddEvents
    backgroundArea
    [ Gdk.EventMaskPointerMotionMask,
      Gdk.EventMaskButtonPressMask,
      Gdk.EventMaskScrollMask
    ]
  Gtk.containerAdd window backgroundArea

  -- geometry <- new Gdk.Geometry [ #minWidth := 500, #minHeight := 500]
  -- screen <- get window #screen
  -- rgbaVisual <- #getRgbaVisual screen
  -- #setVisual window rgbaVisual
  -- surfaceRef <- newIORef (Nothing)

  _ <-
    Gtk.onWidgetDraw
      backgroundArea
      ( \context ->
          renderCairo context (updateBackground backgroundArea stateRef)
            >> pure Gdk.EVENT_STOP
      )

  #showAll window
  gdkWindow <- fromJust <$> Gtk.widgetGetWindow window
  display <- fmap fromJust Gdk.displayGetDefault -- unsafe
  seat <- Gdk.displayGetDefaultSeat display
  device <- fromJust <$> Gdk.seatGetPointer seat -- unsafe
  _ <-
    GLib.timeoutAdd
      GLib.PRIORITY_DEFAULT
      1
      (timeoutCallback inputsRef stateRef gdkWindow device backgroundArea)

  _ <- Gtk.onWidgetButtonPressEvent backgroundArea (backgroundPress inputsRef stateRef)
  _ <- Gtk.onWidgetKeyPressEvent window (keyPress inputsRef)
  _ <- Gtk.onWidgetScrollEvent window (scroll inputsRef)

  #showAll window
  pure ()

main :: IO ()
main = do
  app <- new Gtk.Application []
  _ <- Gio.onApplicationActivate app (startApp app)
  appStatus <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show appStatus)
  pure ()
