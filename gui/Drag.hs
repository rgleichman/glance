{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

-- import Data.Int
-- import qualified Data.Text as T
import Data.Text ()
-- import Debug.Trace (trace, traceIO)
-- import System.Environment (getProgName, getArgs)
import Data.Maybe

import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "Glance"
    , #defaultWidth := 500
    , #defaultHeight := 200
    , #borderWidth := 0
    ]
  scrolledWindow <- new Gtk.ScrolledWindow []
  #add w scrolledWindow
  layout <- new Gtk.Layout [#width := 500, #height := 200]
  --Gtk.widgetAddEvents w [Gdk.EventMaskButtonPressMask]

  #add scrolledWindow layout

  btn <- new Gtk.Button [ #label := "_Hello World!", #useUnderline := True ]

  Gtk.layoutPut layout btn 0 0

  let releaseBtn eventButton = do
        btnWidth <- Gtk.widgetGetAllocatedWidth btn
        btnHeight <- Gtk.widgetGetAllocatedHeight btn
        putStrLn "button released"
        btnAlloc <- Gtk.widgetGetAllocation btn
        oldX <- Gdk.getRectangleX btnAlloc
        oldY <- Gdk.getRectangleY btnAlloc
        x <- Gdk.getEventButtonX eventButton
        y <- Gdk.getEventButtonY eventButton
        let newX = (oldX + (round x) - (btnWidth `div` 2))
        let newY = (oldY + (round y) - (btnHeight `div` 2))
        layoutWidth <- Gtk.getLayoutWidth layout
        layoutHeight <- Gtk.getLayoutHeight layout
        Gtk.setLayoutWidth layout (fromIntegral (newX + btnWidth))
        Gtk.setLayoutHeight layout (fromIntegral (newY + btnHeight))
        Gtk.layoutMove
          layout
          btn
          (oldX + (round x) - (btnWidth `div` 2))
          (oldY + (round y) - (btnHeight `div` 2))
        pure True

  on btn #buttonReleaseEvent (releaseBtn)

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
