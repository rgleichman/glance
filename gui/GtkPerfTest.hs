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
  { _asMoveBtn :: Maybe Gtk.Button
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

updateCanvas canvas state = do
  width  <- realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedWidth  canvas)
  height <- realToFrac <$> (liftIO $ Gtk.widgetGetAllocatedHeight canvas)

  -- setSourceRGB 0 0 0
  -- paint

  (btnX, btnY) <- liftIO $ do
    mMoveBtn <- _asMoveBtn <$> readIORef state
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
  state <- newIORef (AppState{_asMoveBtn=Nothing})
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Glance"
    , #defaultWidth := 500
    , #defaultHeight := 500
    , #borderWidth := 0
    ]
  -- scrolledWindow <- new Gtk.ScrolledWindow [#minContentWidth := 800, #minContentHeight := 800]
  -- #add window scrolledWindow
  -- Gtk.overlayAddOverlay overlay scrolledWindow
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
  GLib.timeoutAdd GLib.PRIORITY_LOW 200 (#queueDraw backgroundArea >> pure True)

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

    renderCairo context (updateCanvas backgroundArea state)
    -- #showAll backgroundArea
    pure True)

  -- #add scrolledWindow layout
  -- #addOverlay overlay layout

  let
    motionCallback eventMotion
      = do
      mMoveBtn <- _asMoveBtn <$> readIORef state
      case mMoveBtn of
        Nothing -> pure ()
        Just btn -> (do
          (x, y) <- getXandY eventMotion
          btnWidth <- Gtk.widgetGetAllocatedWidth btn
          btnHeight <- Gtk.widgetGetAllocatedHeight btn
          Gtk.layoutMove
            layout
            btn
            ((truncate x) - (btnWidth `div` 2))
            ((truncate y) - (btnHeight `div` 2))
          -- #queueDraw backgroundArea
          pure ()

                    )
      pure False
  _ <- on layout #motionNotifyEvent motionCallback

  let
    btnClicked btn
      = do
      putStrLn "Button clicked"
      modifyIORef state
        (\s@AppState{_asMoveBtn}
          -> case _asMoveBtn of
            Nothing -> s{_asMoveBtn=Just btn}
            _ -> s{_asMoveBtn=Nothing}
        )
      btnWidth <- Gtk.widgetGetAllocatedWidth btn
      btnHeight <- Gtk.widgetGetAllocatedHeight btn
      btnAlloc <- Gtk.widgetGetAllocation btn
      (x, y) <- getXandY btnAlloc
      layoutWidth <- Gtk.getLayoutWidth layout
      layoutHeight <- Gtk.getLayoutHeight layout
      Gtk.setLayoutWidth layout (max layoutWidth (fromIntegral (x + btnWidth)))
      Gtk.setLayoutHeight
        layout
        (max layoutHeight (fromIntegral (y + btnHeight)))
      -- Gtk.widgetQueueDraw backgroundArea

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
            newBtn <- new
              Gtk.Button [ #label := "_Node", #useUnderline := True ]
            -- Gtk.widgetAddEvents newBtn
            --   [ Gdk.EventMaskButtonPressMask]
            (#show newBtn)
            (btnWidth, _) <- Gtk.widgetGetPreferredWidth newBtn
            (btnHeight, _) <- Gtk.widgetGetPreferredHeight newBtn
            Gtk.layoutPut
              layout
              newBtn
              ((truncate x) - (btnWidth `div` 2))
              ((truncate y) - (btnHeight `div` 2))
            _ <- on newBtn #clicked (btnClicked newBtn)
            -- Uncomment to stop right clicking on buttons from
            -- creating new buttons at (0, 0). But buttonPressEvent
            -- and buttonReleaseEvent don't seem to be triggered by
            -- the mneumonic.
            --
            -- _ <- on newBtn #buttonPressEvent (\_btn -> btnClicked
            -- newBtn >> pure True)
            (#show newBtn)
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
