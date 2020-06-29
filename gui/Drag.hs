{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

-- import Data.Int
-- import qualified Data.Text as T
import Data.Text ()
-- import Debug.Trace (trace, traceIO)
-- import System.Environment (getProgName, getArgs)
import Data.Maybe
import Data.IORef

import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

data AppState = AppState
  { _asMoveBtn :: Bool
  }

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  state <- newIORef (AppState{_asMoveBtn=False})
  w <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Glance"
    , #defaultWidth := 500
    , #defaultHeight := 500
    , #borderWidth := 0
    ]
  scrolledWindow <- new Gtk.ScrolledWindow []
  #add w scrolledWindow
  layout <- new Gtk.Layout []
  Gtk.widgetAddEvents layout
    [ Gdk.EventMaskPointerMotionMask
    , Gdk.EventMaskButtonPressMask]
  #add scrolledWindow layout
  btn0 <- new Gtk.Button [ #label := "_Hello World!", #useUnderline := True ]
  let
    motionCallback btn eventMotion
      = do
      moveBtn <- _asMoveBtn <$> readIORef state
      if moveBtn
        then (do
          x <- get eventMotion #x
          y <- get eventMotion #y
          btnWidth <- Gtk.widgetGetAllocatedWidth btn
          btnHeight <- Gtk.widgetGetAllocatedHeight btn
          Gtk.layoutMove
            layout
            btn
            ((truncate x) - (btnWidth `div` 2))
            ((truncate y) - (btnHeight `div` 2)))
         else (pure ())
      pure False
  Gtk.layoutPut layout btn0 0 0
  on layout #motionNotifyEvent (motionCallback btn0)

  let
    btnClicked btn
      = do
      modifyIORef state (\s@AppState{_asMoveBtn} -> s{_asMoveBtn=not _asMoveBtn})
      btnWidth <- Gtk.widgetGetAllocatedWidth btn
      btnHeight <- Gtk.widgetGetAllocatedHeight btn
      btnAlloc <- Gtk.widgetGetAllocation btn
      x <- get btnAlloc #x
      y <- get btnAlloc #y
      layoutWidth <- Gtk.getLayoutWidth layout
      layoutHeight <- Gtk.getLayoutHeight layout
      Gtk.setLayoutWidth layout (fromIntegral (x + btnWidth))
      Gtk.setLayoutHeight layout (fromIntegral (y + btnHeight))

  -- buttonPressEvent and buttonReleaseEvent don't seem to be triggered by the
  -- mneumonic.
  on btn0 #clicked (btnClicked btn0)
  on layout #buttonPressEvent $ \ _layout -> do
    putStrLn "layout clicked, adding btn"
    -- newBtn <- new Gtk.Button [ #label := "_New!", #useUnderline := True ]
    -- Gtk.layoutPut layout newBtn 0 0
    pure False

  #showAll w
  pure ()

main :: IO ()
main = do
  app <- new Gtk.Application []
  on app #activate $ do
    activateApp app
    pure ()
  status <- Gio.applicationRun app Nothing
  print status
  pure ()
