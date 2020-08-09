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
import Data.Maybe (fromJust, fromMaybe)
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

-- | A graphical element that can be clicked
data Element = Element
  { _elPosition :: !(Double, Double) -- ^ (x, y) of top left corner
  , _elSize :: !(Double, Double)  -- ^ (width, height)
  , _elZ :: Int -- ^ Depth. Higher values are drawn on top
  -- _elZ is currently ignored
  }

data AppState = AppState
  { _asMovingNode :: Maybe Int -- ^ _asElements key
  , _asEdges :: [(Element, Element)]
  , _asMouseXandY :: !(Double, Double)
  , _asElements :: IntMap.IntMap Element
  , _asTime :: SystemTime
  , _asFPSr :: Double -- ^ FPS rouned down to nearest hundred if over 200 fps.
  }

emptyAppState :: AppState
emptyAppState = AppState
  { _asMovingNode = Nothing
  , _asEdges = []
  , _asMouseXandY = (0, 0)
  , _asElements = mempty
  , _asTime = MkSystemTime 0 0
  , _asFPSr = 0
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

drawNode :: Element -> Render ()
drawNode Element{..} = do
  let
    (x, y) = _elPosition
    (width, height) = _elSize

  setSourceRGB 1 0 0
  setLineWidth 1
  rectangle x y width height
  stroke

updateBackground :: p -> IORef AppState -> Render (IntMap.IntMap ())
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
  traverse drawNode (_asElements stateVal)

findElementByPosition :: IntMap.IntMap Element -> (Double, Double) -> Maybe Int
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
    fst <$> find mouseInElement (IntMap.toList elements)

-- Update the state based on the old state and the mouse
-- position. Consider moving inputs like mouse position into a
-- separate input struct.
updateState :: AppState -> AppState
updateState oldState@AppState{_asMouseXandY, _asElements, _asMovingNode} =
  let
    -- Move the asMovingNode to MouseXandY
    newElements = case _asMovingNode of
      Nothing -> _asElements
      Just nodeId -> IntMap.adjust
        (\oldNode@Element{_elPosition, _elSize} ->
          let
            newX = fst _asMouseXandY - (fst _elSize / 2)
            newY = snd _asMouseXandY - (snd _elSize / 2)
          in
            oldNode{_elPosition=(newX, newY)})
        nodeId
        _asElements
    newState = oldState{_asElements=newElements}
  in
    newState

startApp :: Gtk.Application -> IO ()
startApp app = do
  stateRef <- newIORef emptyAppState
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
  let
    timeoutCallback :: IO Bool
    timeoutCallback = do
      -- TODO Move this time stuff into a function.
      newTime@(MkSystemTime seconds nanoseconds) <- getSystemTime
      oldState <- readIORef stateRef
      let
        (MkSystemTime oldSeconds oldNanoseconds) = _asTime oldState
        secondsDiff = seconds - oldSeconds
        nanosecondDiff = nanoseconds - oldNanoseconds
        fps = if secondsDiff == 0
          then fromIntegral (div (10^(9 :: Int)) nanosecondDiff)
          else 1 / fromIntegral secondsDiff
        truncatedFps = if fps >= 200
          then fromIntegral $ div (truncate fps) 100 * (100 :: Int)
          else fps
      gdkDevicePosition <- Gdk.windowGetDevicePositionDouble gdkWindow device
      let (_, x, y, _) = gdkDevicePosition

      modifyIORef' stateRef
        (\s@AppState{_asMouseXandY}
         -> s{_asMouseXandY=(x, y)
             , _asTime=newTime
             , _asFPSr=truncatedFps}
        )
      modifyIORef' stateRef updateState

      #queueDraw backgroundArea
      pure True

  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1 timeoutCallback

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
          -- print (x, y)

          modifyIORef' stateRef
            (\s@AppState{_asMovingNode, _asElements}
            ->
              let
                newMovingNode = findElementByPosition _asElements mousePosition
              in
                s{_asMovingNode=newMovingNode}
            )

          movingNode <- _asMovingNode <$> readIORef stateRef
          print movingNode
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
