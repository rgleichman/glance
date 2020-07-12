{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Drag (mainDrag) where

import Control.Concurrent
import Control.Monad
import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef

import Data.GI.Base
import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as GP
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

data AppState = AppState
  { _asMovingNode :: Maybe Gtk.ListBox
  , _asOutBtn :: Maybe Gtk.Button
  , _asEdges :: [(Gtk.Button, Gtk.Button)]
  }

renderCairo c r = withManagedPtr c $ \pointer ->
  runReaderT (runRender r) (Cairo (castPtr pointer))

-- TODO Add type signature
getXandY event =
  (\x y -> (x, y)) <$> get event #x <*> get event #y

-- updateCanvas :: WidgetClass widget => widget -> PangoLayout -> Render ()
drawLine (fromX, fromY) (toX, toY) = do
  setSourceRGB 0 1 0
  setLineWidth 5

  moveTo fromX fromY
  lineTo toX toY
  stroke

updateBackground canvas state = do
  width  <- realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedWidth  canvas)
  height <- realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedHeight canvas)

  -- TODO This should be moved into the setup phase
  setSourceRGB 0 0 0
  paint

  (btnX, btnY) <- liftIO $ do
    mMoveBtn <- _asMovingNode <$> readIORef state
    case mMoveBtn of
        Nothing -> pure (0, 0)
        Just btn -> (do
          btnAlloc <- Gtk.widgetGetAllocation btn
          (\(x, y) -> (realToFrac x, realToFrac y)) <$> getXandY btnAlloc)
  --drawLine (0, 0) (width, height)
  drawLine (0, 0) (btnX, btnY)
  --drawLine (width, 0) (btnX, btnY)
  -- setSourceRGB 0 1 0
  -- setLineWidth 5

  -- moveTo 0 0
  -- lineTo width height
  -- stroke

startApp :: Gtk.Application -> IO ()
startApp app = do
  state <- newIORef (AppState{_asMovingNode=Nothing, _asOutBtn=Nothing, _asEdges=[]})
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Glance"
    , #defaultWidth := 500
    , #defaultHeight := 500
    , #borderWidth := 0
    ]
  overlay <- new Gtk.Overlay []
  backgroundArea <- new Gtk.DrawingArea []
  layout <- new Gtk.Layout []
  Gtk.widgetAddEvents layout
    [ Gdk.EventMaskPointerMotionMask
    , Gdk.EventMaskButtonPressMask]
  #add window overlay
  #add overlay backgroundArea
  #addOverlay overlay layout


  geometry <- new Gdk.Geometry [ #minWidth := 500, #minHeight := 500]

  -- screen <- get window #screen
  -- rgbaVisual <- #getRgbaVisual screen
  -- #setVisual window rgbaVisual

  -- No noticable change with setting this to GLib.PRIORITY_DEFAULT
  -- GLib.timeoutAdd GLib.PRIORITY_LOW 10 (#queueDraw backgroundArea >> pure True)

  surfaceRef <- newIORef (Nothing)

  _ <- on backgroundArea #draw (\context -> do
    mSurface <- readIORef surfaceRef
    surface <- case mSurface of
      Nothing -> do
        (width, height) <- #getSize window
        surf <- createImageSurface
          FormatARGB32
          (fromIntegral width)
          (fromIntegral height)
        writeIORef surfaceRef $ Just $ surf
        pure surf
      Just surface -> pure surface

    renderCairo context (updateBackground backgroundArea state)
    -- #showAll backgroundArea
    pure True)

  -- #add scrolledWindow layout
  -- #addOverlay overlay layout

  let
    motionCallback eventMotion
      = do
      mMoveNode <- _asMovingNode <$> readIORef state
      case mMoveNode of
        Nothing -> pure ()
        Just node -> (do
          (x, y) <- getXandY eventMotion
          btnWidth <- Gtk.widgetGetAllocatedWidth node
          btnHeight <- Gtk.widgetGetAllocatedHeight node
          Gtk.layoutMove
            layout
            node
            ((truncate x) - (btnWidth `div` 2))
            ((truncate y) - (btnHeight `div` 2))
          -- #queueDraw backgroundArea
          pure ()

                    )
      pure False
  _ <- on layout #motionNotifyEvent motionCallback

  let
    moveBtnClicked node
      = do
      putStrLn "Move button clicked"

      mMoveNode <- _asMovingNode <$> readIORef state
      case mMoveNode of
        Just _ ->
          Gtk.widgetQueueDraw backgroundArea
        _ -> pure ()

      modifyIORef state
        (\s@AppState{_asMovingNode}
          -> case _asMovingNode of
            Nothing -> s{_asMovingNode=Just node}
            _ -> s{_asMovingNode=Nothing}
        )
      -- nodeWidth <- Gtk.widgetGetAllocatedWidth node
      -- nodeHeight <- Gtk.widgetGetAllocatedHeight node
      -- nodeAlloc <- Gtk.widgetGetAllocation node
      -- (x, y) <- getXandY nodeAlloc
      -- layoutWidth <- Gtk.getLayoutWidth layout
      -- layoutHeight <- Gtk.getLayoutHeight layout
      -- Gtk.setLayoutWidth layout (max layoutWidth (fromIntegral (x + nodeWidth)))
      -- Gtk.setLayoutHeight
      --   layout
      --   (max layoutHeight (fromIntegral (y + nodeHeight)))

  -- buttonPressEvent and buttonReleaseEvent don't seem to be triggered by the
  -- mneumonic.
  _ <- on layout #buttonPressEvent $ \ eventButton ->
    do
      putStrLn "layout clicked"
      mouseBtn <- get eventButton #button
      -- button 3 is usally right mouse button
      (when (mouseBtn == 3)
        (do
            (x, y) <- getXandY eventButton
            moveBtn <- new
              Gtk.Button [ #label := "_Move", #useUnderline := True ]
            outBtn <- new
              Gtk.Button [ #label := "_Out", #useUnderline := True ]
            inBtn <- new
              Gtk.Button [ #label := "_In", #useUnderline := True ]
            -- Gtk.widgetAddEvents newBtn
            --   [ Gdk.EventMaskButtonPressMask]
            listBox <- new Gtk.ListBox []
            #add listBox inBtn
            #add listBox moveBtn
            #add listBox outBtn

            (#showAll listBox)
            (listBoxWidth, _) <- Gtk.widgetGetPreferredWidth listBox
            (listBoxHeight, _) <- Gtk.widgetGetPreferredHeight listBox
            Gtk.layoutPut
              layout
              listBox
              ((truncate x) - (listBoxWidth `div` 2))
              ((truncate y) - (listBoxHeight `div` 2))
            _ <- on moveBtn #clicked (moveBtnClicked listBox)
            -- Uncomment to stop right clicking on buttons from
            -- creating new buttons at (0, 0). But buttonPressEvent
            -- and buttonReleaseEvent don't seem to be triggered by
            -- the mneumonic.
            --
            -- _ <- on newBtn #buttonPressEvent (\_btn -> moveBtnClicked
            -- newBtn >> pure True)
            (#showAll listBox)
            pure ()))
      pure False
  #showAll window
  pure ()

mainDrag :: IO ()
mainDrag = do
  app <- new Gtk.Application []
  _ <- on app #activate (startApp app)
  status <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show status)
  pure ()
