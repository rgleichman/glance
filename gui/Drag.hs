{-# LANGUAGE OverloadedStrings, OverloadedLabels, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Drag (mainDrag) where

import Control.Monad
import Data.IORef

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

data AppState = AppState
  { _asMoveBtn :: Maybe Gtk.Button
  }

-- TODO Add type signature
getXandY event =
  (\x y -> (x, y)) <$> get event #x <*> get event #y

startApp :: Gtk.Application -> IO ()
startApp app = do
  state <- newIORef (AppState{_asMoveBtn=Nothing})
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
            ((truncate y) - (btnHeight `div` 2)))
      pure False
  _ <- on layout #motionNotifyEvent motionCallback

  let
    btnClicked btn
      = do
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

  -- buttonPressEvent and buttonReleaseEvent don't seem to be triggered by the
  -- mneumonic.
  _ <- on layout #buttonPressEvent $ \ eventButton ->
    do
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
  #showAll w
  pure ()

mainDrag :: IO ()
mainDrag = do
  app <- new Gtk.Application []
  _ <- on app #activate (startApp app)
  status <- Gio.applicationRun app Nothing
  putStrLn ("Application status is " <> show status)
  pure ()
