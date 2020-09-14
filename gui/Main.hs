-- This file is formatted with Ormolu
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Data.GI.Base (AttrOp ((:=)), get, new, on, withManagedPtr)
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

nodeSize :: (Double, Double)
nodeSize = (100, 40)

newtype ElemId = ElemId {_unElemId :: Int} deriving (Show, Eq, Ord)

-- | A graphical element that can be clicked
data Element = Element
  { -- | (x, y) of top left corner
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

data AppState = AppState
  { -- | _asElements key
    _asMovingNode :: Maybe ElemId,
    _asEdges :: [(Element, Element)],
    _asElements :: IntMap.IntMap Element,
    -- | FPS rouned down to nearest hundred if over 200 fps.
    _asFPSr :: Double
  }

data InputEvent
  = -- Which node was clicked and the relative click position within a node.
    ClickOnNode
      ElemId
      (Double, Double) -- relative mouse position
      Word32 -- mouse button
  | AddNode (Double, Double) -- where to add the node

emptyAppState :: AppState
emptyAppState =
  AppState
    { _asMovingNode = Nothing,
      _asEdges = [],
      _asElements = mempty,
      _asFPSr = 0
    }

emptyInputs :: Inputs
emptyInputs =
  Inputs
    { _inMouseXandY = (0, 0),
      _inTime = MkSystemTime 0 0,
      _inPrevTime = MkSystemTime 0 0,
      _inEvents = mempty
    }

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

drawNode :: (Int, Element) -> Render ()
drawNode (elemId, Element {..}) = do
  let (x, y) = _elPosition
      (width, height) = _elSize
      halfWidth = width / 2

  Cairo.setSourceRGB 1 0 0
  Cairo.setLineWidth 1
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
  traverse_ drawNode (IntMap.toList (_asElements stateVal))

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

-- | Add a node to the canvas at the given position.
addNode :: (Double, Double) -> AppState -> AppState
addNode addPosition s@AppState {_asElements} =
  let biggestKey = maybe 0 fst (IntMap.lookupMax _asElements)
      newNode =
        Element
          { _elPosition = addPosition,
            _elSize = nodeSize,
            _elZ = 0
          }
      newElements =
        IntMap.insert (biggestKey + 1) newNode _asElements
   in s {_asElements = newElements}

processInput :: InputEvent -> AppState -> AppState
processInput inputEvent oldState@AppState {_asMovingNode} =
  case inputEvent of
    ClickOnNode elemId _relativePosition _mouseBtn ->
      let newMovingNodeId = case _asMovingNode of
            Nothing -> Just elemId
            Just _ -> Nothing
       in oldState {_asMovingNode = newMovingNodeId}
    AddNode addPosition -> addNode addPosition oldState

processInputs :: Inputs -> AppState -> AppState
processInputs
  Inputs {_inEvents}
  oldState@AppState {_asElements, _asMovingNode} =
    let compose = foldr (.) id
     in compose (fmap processInput _inEvents) oldState

-- | Update the state based on the old state and the mouse
-- position. Consider moving inputs like mouse position into a
-- separate input struct.
updateState :: Inputs -> AppState -> AppState
updateState
  inputs@Inputs {_inMouseXandY, _inEvents}
  oldState@AppState {_asElements, _asMovingNode} =
    let -- Move the asMovingNode to MouseXandY
        newElements = case _asMovingNode of
          Nothing -> _asElements
          Just nodeId ->
            IntMap.adjust
              ( \oldNode@Element {_elPosition, _elSize} ->
                  let newPosition =
                        elementwiseOp
                          (-)
                          _inMouseXandY
                          (Tuple.both (/ 2) _elSize)
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
  let (_, x, y, _) = gdkDevicePosition

  modifyIORef'
    inputsRef
    ( \inputs@Inputs {_inTime} ->
        inputs
          { _inMouseXandY = (x, y),
            _inTime = newTime,
            _inPrevTime = _inTime
          }
    )

  inputs <- readIORef inputsRef

  -- TODO Refactor processInputs and updateState so that there is a
  -- single event stream. Events are processed in order. Processing an
  -- event may modify the state.
  modifyIORef' stateRef (processInputs inputs)
  modifyIORef' stateRef (updateState inputs)
  modifyIORef' inputsRef (\i -> i {_inEvents = []})

  Gtk.widgetQueueDraw backgroundArea
  pure True

leftClickAction ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  Word32 ->
  IO ()
leftClickAction inputsRef stateRef eventButton mouseBtn =
  do
    mousePosition <- getXandY eventButton
    state <- readIORef stateRef

    let mElem = findElementByPosition (_asElements state) mousePosition

        addClickEvent inputs@Inputs {_inEvents} =
          case mElem of
            Nothing -> inputs
            Just (elemId, element) ->
              -- TODO NOW Make function to add to event queue.
              inputs
                { _inEvents =
                    ClickOnNode
                      (ElemId elemId)
                      (elementwiseOp (-) mousePosition (_elPosition element))
                      mouseBtn :
                    _inEvents
                }
    modifyIORef'
      inputsRef
      addClickEvent

rightClickAction ::
  IORef Inputs ->
  Gdk.EventButton ->
  IO ()
rightClickAction inputsRef eventButton =
  do
    (x, y) <- getXandY eventButton

    modifyIORef'
      inputsRef
      ( \inputs@Inputs {_inEvents} ->
          inputs {_inEvents = AddNode (x, y) : _inEvents}
      )
    pure ()

backgroundPress ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  IO Bool
backgroundPress inputsRef stateRef eventButton = do
  mouseBtnNum <- get eventButton #button
  let mouseButton = toMouseButton mouseBtnNum
  putStrLn ("Background pressed by " <> show mouseButton)
  case toMouseButton mouseBtnNum of
    RightMouseButton -> rightClickAction inputsRef eventButton
    LeftMouseButton ->
      leftClickAction inputsRef stateRef eventButton mouseBtnNum
    _ -> mempty
  pure True

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
  Gtk.widgetAddEvents
    backgroundArea
    [ Gdk.EventMaskPointerMotionMask,
      Gdk.EventMaskButtonPressMask
    ]
  #add window backgroundArea

  -- geometry <- new Gdk.Geometry [ #minWidth := 500, #minHeight := 500]

  -- screen <- get window #screen
  -- rgbaVisual <- #getRgbaVisual screen
  -- #setVisual window rgbaVisual

  -- surfaceRef <- newIORef (Nothing)

  _ <-
    on
      backgroundArea
      #draw
      ( \context ->
          renderCairo context (updateBackground backgroundArea stateRef)
            >> pure True
      )

  #showAll window
  gdkWindow <- fromJust <$> #getWindow window
  display <- fmap fromJust Gdk.displayGetDefault -- TODO unsafe
  seat <- Gdk.displayGetDefaultSeat display
  device <- fromJust <$> Gdk.seatGetPointer seat -- TODO unsafe
  _ <-
    GLib.timeoutAdd
      GLib.PRIORITY_DEFAULT
      1
      (timeoutCallback inputsRef stateRef gdkWindow device backgroundArea)

  _ <- on backgroundArea #buttonPressEvent (backgroundPress inputsRef stateRef)

  #showAll window
  pure ()

main :: IO ()
main = do
  app <- new Gtk.Application []
  _ <- on app #activate (startApp app)
  appStatus <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show appStatus)
  pure ()
