{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)

import qualified GI.Gtk as GTK
import qualified GI.Gdk as Gdk

import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data AppState = AppState
  { mouseX :: Int
  , mouseY :: Int
  }

data AppEvent = Closed | MouseDown (Int, Int)

appView :: AppState -> AppView GTK.Window AppEvent
appView state =
  let
    label = widget GTK.Label [#label := "Glance", #xalign := (0.8)]
    handleButton :: Gdk.EventMotion -> GTK.Window -> IO (Bool, AppEvent)
    handleButton eventMotion _ = do
      x <- Gdk.getEventMotionX eventMotion
      y <- Gdk.getEventMotionY eventMotion
      pure (True, MouseDown (floor x, floor y))
    paddingX = min 300 (fromIntegral (mouseX state))
  in
    bin
    GTK.Window
    [ #title := "Glance"
    , on #deleteEvent (const (True, Closed))
    , onM #motionNotifyEvent handleButton
    , #widthRequest := 500
    , #heightRequest := 500
    ]
    -- $ widget GTK.Label [#label := "Glance", #xalign := (0.8)]
    $ container GTK.Box [] [BoxChild defaultBoxChildProperties {padding=paddingX} label]

appUpdate :: AppState -> AppEvent -> Transition AppState AppEvent
appUpdate _ event = case event of
  Closed -> Exit
  MouseDown (x, y) -> Transition (AppState x y) (pure Nothing)

main :: IO ()
main = void $ run App
  { view = appView
  , update = appUpdate
  , inputs = []
  , initialState = AppState 0 0
  }
