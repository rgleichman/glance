-- This file is formatted with Ormolu
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | All the GTK and GDK code goes in this module.
module GtkGui (gtkMain) where

-- import qualified GI.GdkPixbuf as GP

-- import Debug.Trace (trace)

import Control.Monad.IO.Class (MonadIO)
import Data.GI.Base (AttrOp ((:=)), new)
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime)
import GHC.Word (Word32)
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import GuiInternals
  ( AppState,
    Inputs,
    KeyEvent (AbortKey, TranslateKey, UndoKey, UnknownKey),
    MouseButton (UnknownMouseButton),
    backgroundPress,
    emptyAppState,
    emptyInputs,
    keyPress,
    keyRelease,
    renderCairo,
    scroll,
    step,
    updateBackground,
  )

--------- Constants -------------
translateKey :: Text
translateKey = " "

-- | A mapping between mouse button names and the GTK
-- mouse button numbers via Enum, so order is important!
-- mouseButtonNum :: MouseButton -> Word32
-- mouseButtonNum = fromIntegral . (+ 1) . fromEnum

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

getXandY ::
  MonadIO f =>
  Gdk.EventButton ->
  f (Double, Double)
getXandY event =
  (\x y -> (x, y)) <$> Gdk.getEventButtonX event <*> Gdk.getEventButtonY event

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
  step
    inputsRef
    stateRef
    newTime
    (mouseX, mouseY)
    (Gtk.widgetQueueDraw backgroundArea)
  pure Gdk.EVENT_STOP

backgroundPressCallback ::
  IORef Inputs ->
  IORef AppState ->
  Gdk.EventButton ->
  IO Bool
backgroundPressCallback inputsRef stateRef eventButton = do
  mouseButton <- toMouseButton <$> Gdk.getEventButtonButton eventButton
  rawMousePosition <- getXandY eventButton
  backgroundPress inputsRef stateRef mouseButton rawMousePosition
  pure Gdk.EVENT_STOP

keyPressCallback :: IORef Inputs -> IORef AppState -> Gdk.EventKey -> IO Bool
keyPressCallback inputsRef stateRef eventKey = do
  -- TODO May want to check that ctrl is pressed by checking that
  -- getEventKeyState is ModifierTypeControlMask. May also want to use
  -- Gdk.KEY_?.
  key <- Gdk.getEventKeyString eventKey
  let keyEvent = case key of
        Nothing -> UnknownKey ""
        Just "\SUB" -> UndoKey -- ctrl-z pressed
        Just "\a" -> AbortKey -- ctrl-g
        Just str ->
          if
              | str == translateKey -> TranslateKey
              | otherwise -> UnknownKey str
  keyPress inputsRef stateRef keyEvent
  pure Gdk.EVENT_STOP

keyReleaseCallback :: IORef Inputs -> Gdk.EventKey -> IO Bool
keyReleaseCallback inputsRef eventKey = do
  -- TODO May want to check that ctrl is pressed by checking that
  -- getEventKeyState is ModifierTypeControlMask. May also want to use
  -- Gdk.KEY_?.
  key <- Gdk.getEventKeyString eventKey
  let keyEvent =
        if
            | key == Just translateKey -> TranslateKey
            | otherwise -> UnknownKey ""
  keyRelease inputsRef keyEvent
  pure Gdk.EVENT_STOP

scrollCallback :: IORef Inputs -> Gdk.EventScroll -> IO Bool
scrollCallback inputsRef scrollEvent = do
  -- scale in (zooming in) is negative (usually -1), scale out
  -- (zooming out) is positive (usually 1)
  deltaY <- Gdk.getEventScrollDeltaY scrollEvent
  scroll inputsRef deltaY
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
          renderCairo context (updateBackground backgroundArea inputsRef stateRef)
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

  _ <-
    Gtk.onWidgetButtonPressEvent
      backgroundArea
      (backgroundPressCallback inputsRef stateRef)
  _ <- Gtk.onWidgetKeyPressEvent window (keyPressCallback inputsRef stateRef)
  _ <- Gtk.onWidgetKeyReleaseEvent window (keyReleaseCallback inputsRef)
  _ <- Gtk.onWidgetScrollEvent window (scrollCallback inputsRef)

  #showAll window
  pure ()

gtkMain :: IO ()
gtkMain = do
  app <- new Gtk.Application []
  _ <- Gio.onApplicationActivate app (startApp app)
  appStatus <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show appStatus)
  pure ()
