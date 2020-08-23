{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Coerce
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef
import Data.List
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust)
import Data.Time.Clock.System
import Foreign.Ptr (castPtr)
import GHC.Word (Word32)

import Data.GI.Base
import qualified GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
-- import qualified GI.GdkPixbuf as GP
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

leftMouseButton :: Word32
leftMouseButton = 1

rightMouseButton :: Word32
rightMouseButton = 3

nodeSize :: (Double, Double)
nodeSize = (100, 40)

-- TODO USE newtype for element ID.
newtype ElemId = ElemId { _unElemId :: Int } deriving (Show, Eq, Ord)

-- | A graphical element that can be clicked
data Element = Element
  { _elPosition :: !(Double, Double) -- ^ (x, y) of top left corner
  , _elSize :: !(Double, Double)  -- ^ (width, height)
  , _elZ :: Int -- ^ Depth. Higher values are drawn on top
  -- _elZ is currently ignored
  }

data Inputs = Inputs
  { _inMouseXandY :: !(Double, Double)
  , _inTime :: SystemTime
  , _inPrevTime :: SystemTime
  , _inEvents :: [InputEvents]
  }

data AppState = AppState
  { _asMovingNode :: Maybe ElemId -- ^ _asElements key
  , _asEdges :: [(Element, Element)]
  , _asElements :: IntMap.IntMap Element
  , _asFPSr :: Double -- ^ FPS rouned down to nearest hundred if over 200 fps.
  }

data InputEvents =
  -- Which node was clicked and the relative click position within a node.
  ClickOnNode
    ElemId
    (Double, Double) -- relative mouse position
    Word32 -- mouse button

emptyAppState :: AppState
emptyAppState = AppState
  { _asMovingNode = Nothing
  , _asEdges = []
  , _asElements = mempty
  , _asFPSr = 0
  }

emptyInputs :: Inputs
emptyInputs = Inputs
  { _inMouseXandY = (0, 0)
  , _inTime = MkSystemTime 0 0
  , _inPrevTime = MkSystemTime 0 0
  , _inEvents = mempty
  }

renderCairo :: Coercible a (ManagedPtr ()) => a -> Render c -> IO c
renderCairo c r = withManagedPtr c $ \pointer ->
  runReaderT (runRender r) (Cairo (castPtr pointer))

getXandY
  :: MonadIO f
  => Gdk.EventButton -> f (Double, Double)
getXandY event =
  (\x y -> (x, y)) <$> Gdk.getEventButtonX event <*> Gdk.getEventButtonY event

_drawLine :: (Double, Double) -> (Double, Double) -> Render ()
_drawLine (fromX, fromY) (toX, toY) = do
  setSourceRGB 0 1 0
  setLineWidth 5

  moveTo fromX fromY
  lineTo toX toY
  stroke

_drawCircle :: (Double, Double) -> Render ()
_drawCircle (x, y) = do
--   setSourceRGB 1 0 0
  setLineWidth 1
  -- moveTo x y
  let radius = 20
  let tau = 2 * pi
  arc x y radius 0 tau
  stroke

drawNode :: (Int, Element) -> Render ()
drawNode (elemId, Element{..}) = do
  let
    (x, y) = _elPosition
    (width, height) = _elSize
    halfWidth = width / 2

  setSourceRGB 1 0 0
  setLineWidth 1
  rectangle x y halfWidth height
  showText (show elemId)
  stroke
  setSourceRGB 0 1 0
  rectangle (x + halfWidth) y halfWidth height
  stroke

updateBackground :: p -> IORef AppState -> Render (())
updateBackground _canvas stateRef = do
  -- width  <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedWidth  canvas)
  --             :: Render Double)
  -- height <- (realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedHeight canvas)
  --             :: Render Double)

  -- TODO This should be moved into the setup phase
  setSourceRGB 0 0 0
  paint

  stateVal <- liftIO $ readIORef stateRef
  setSourceRGB 1 1 1
  moveTo 10 10
  showText ("fps=" <> show (_asFPSr stateVal))
  setSourceRGB 1 0 0
  _ <- traverse drawNode (IntMap.toList (_asElements stateVal))
  pure ()

findElementByPosition :: IntMap.IntMap Element -> (Double, Double) -> Maybe (Int, Element)
findElementByPosition elements (mouseX, mouseY) =
  let
    mouseInElement (_elementId, Element{_elPosition, _elSize}) =
      let
        (x, y) = _elPosition
        (width, height) = _elSize
      in
        mouseX >= x && mouseX <= (x + width) &&
        mouseY >= y && mouseY <= (y + height)
  in
    find mouseInElement (IntMap.toList elements)

getFps :: Inputs -> Double
getFps inputs =
  let
    (MkSystemTime seconds nanoseconds) = _inTime inputs
    (MkSystemTime oldSeconds oldNanoseconds) = _inPrevTime inputs
    secondsDiff = seconds - oldSeconds
    nanosecondDiff = nanoseconds - oldNanoseconds
    fps = if secondsDiff == 0
      then fromIntegral (div (10^(9 :: Int)) nanosecondDiff)
      else 1 / fromIntegral secondsDiff
  in
    if fps >= 200
      then fromIntegral $ div (truncate fps) 100 * (100 :: Int)
      else fps


processInputs :: Inputs -> AppState -> AppState
processInputs Inputs{_inEvents} oldState@AppState{_asElements, _asMovingNode} =
  let
    compose = foldr (.) id
  in
    compose (fmap processInput _inEvents) oldState

processInput :: InputEvents -> AppState -> AppState
processInput inputEvent oldState@AppState{_asElements, _asMovingNode} =
  case inputEvent of
    -- TODO only change movingNode if mouseBtn is leftClick
    ClickOnNode elemId _relativePosition _mouseBtn ->
      let
        newMovingNodeId = case _asMovingNode of
          Nothing -> Just elemId
          Just _ -> Nothing
      in
        oldState{_asMovingNode=newMovingNodeId}

-- Update the state based on the old state and the mouse
-- position. Consider moving inputs like mouse position into a
-- separate input struct.
updateState :: Inputs -> AppState -> AppState
updateState inputs@Inputs{_inMouseXandY, _inEvents} oldState@AppState{_asElements, _asMovingNode} =
  let
    -- Move the asMovingNode to MouseXandY
    newElements = case _asMovingNode of
      Nothing -> _asElements
      Just nodeId -> IntMap.adjust
        (\oldNode@Element{_elPosition, _elSize} ->
          let
            newX = fst _inMouseXandY - (fst _elSize / 2)
            newY = snd _inMouseXandY - (snd _elSize / 2)
          in
            oldNode{_elPosition=(newX, newY)})
        (_unElemId nodeId)
        _asElements


    newState = oldState
      { _asElements=newElements
      , _asFPSr=getFps inputs
      }
  in
    newState

timeoutCallback ::
  IORef Inputs
  -> IORef AppState
  -> Gdk.Window
  -> Gdk.Device
  -> Gtk.DrawingArea
  -> IO Bool
timeoutCallback inputsRef stateRef gdkWindow device backgroundArea = do
  newTime <- getSystemTime
  gdkDevicePosition <- Gdk.windowGetDevicePositionDouble gdkWindow device
  let (_, x, y, _) = gdkDevicePosition

  modifyIORef' inputsRef
    (\inputs@Inputs{_inTime}
      -> inputs{_inMouseXandY=(x, y)
               , _inTime=newTime
               , _inPrevTime=_inTime
               })

  inputs <- readIORef inputsRef

  modifyIORef' stateRef (processInputs inputs)
  modifyIORef' stateRef (updateState inputs)
  modifyIORef' inputsRef (\i -> i{_inEvents=[]})

  Gtk.widgetQueueDraw backgroundArea
  pure True

startApp :: Gtk.Application -> IO ()
startApp app = do
  stateRef <- newIORef emptyAppState
  inputsRef <- newIORef emptyInputs
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Glance"
    , #defaultWidth := 500
    , #defaultHeight := 500
    , #borderWidth := 0
    ]
  backgroundArea <- new Gtk.DrawingArea []
  Gtk.widgetAddEvents backgroundArea
    [ Gdk.EventMaskPointerMotionMask
    , Gdk.EventMaskButtonPressMask]
  #add window backgroundArea

  -- geometry <- new Gdk.Geometry [ #minWidth := 500, #minHeight := 500]

  -- screen <- get window #screen
  -- rgbaVisual <- #getRgbaVisual screen
  -- #setVisual window rgbaVisual

  -- surfaceRef <- newIORef (Nothing)

  _ <- on backgroundArea #draw (\context -> do
    -- mSurface <- readIORef surfaceRef
    -- surface <- case mSurface of
    --   Nothing -> do
    --     (width, height) <- #getSize window
    --     surf <- createImageSurface
    --       FormatARGB32
    --       (fromIntegral width)
    --       (fromIntegral height)
    --     writeIORef surfaceRef $ Just $ surf
    --     pure surf
    --   Just surface -> pure surface

    _ <- renderCairo context (updateBackground backgroundArea stateRef)
    pure True)

  #showAll window
  gdkWindow <- fromJust <$> #getWindow window
  display <- fmap fromJust Gdk.displayGetDefault -- TODO unsafe
  deviceManager <- fromJust <$> Gdk.displayGetDeviceManager display -- TODO deprecated
  device <- Gdk.deviceManagerGetClientPointer deviceManager
  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1 (timeoutCallback inputsRef stateRef gdkWindow device backgroundArea)

  let
    backgroundPress eventButton = do
      mouseBtn <- get eventButton #button
      when (mouseBtn == rightMouseButton)
        (do
          (x, y) <- getXandY eventButton

          modifyIORef' stateRef
            (\s@AppState{_asElements}
            ->
              let
                key = maybe 0 fst (IntMap.lookupMax _asElements)
                newNode = Element
                  { _elPosition = (x, y)
                  , _elSize = nodeSize
                  , _elZ = 0
                  }
                newElements = IntMap.insert (key + 1) newNode _asElements
              in
                s{_asElements=newElements}
            )
          pure ()
        )
      when (mouseBtn == leftMouseButton)
        (do
          putStrLn "Left click"
          mousePosition <- getXandY eventButton

          state <- readIORef stateRef

          modifyIORef' inputsRef
            (\s@Inputs{_inEvents}
            ->
              let
                mElem = findElementByPosition (_asElements state) mousePosition
              in
                case mElem of
                  Nothing -> s
                  Just (elemId, element) ->
                    let
                      (elementX, elementY) = _elPosition element
                      (mouseX, mouseY) = mousePosition
                    in
                      s{_inEvents
                        =ClickOnNode (ElemId elemId) (mouseX - elementX, mouseY - elementY) mouseBtn
                         : _inEvents}
            )
            -- (\s@AppState{_asMovingNode, _asElements}
            -- ->
            --   let
            --     -- toggle _asMovingNode when clicked
            --     newMovingNode = case _asMovingNode of
            --       Nothing -> findElementByPosition _asElements mousePosition
            --       Just _ -> Nothing
            --   in
            --     s{_asMovingNode=newMovingNode}
            -- )

          -- movingNode <- _asMovingNode <$> readIORef stateRef
          -- print movingNode
        )

      putStrLn "backgroundPressed"
      pure True
  _ <- on backgroundArea #buttonPressEvent backgroundPress

  #showAll window
  pure ()

main :: IO ()
main = do
  app <- new Gtk.Application []
  _ <- on app #activate (startApp app)
  appStatus <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show appStatus)
  pure ()
