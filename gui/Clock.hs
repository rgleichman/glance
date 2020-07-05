{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- original author:
--    Mirco "MacSlow" Mueller <macslow@bangang.de>
--
-- created:
--    10.1.2006 (or so)
--
-- http://www.gnu.org/licenses/licenses.html#GPL
--
-- ported to Haskell (gtk2hs) by:
--    Duncan Coutts <duncan.coutts@worc.ox.ac.uk>
--
-- ported to haskell-gi from the gtk2hs port by:
--    Iñaki García Etxebarria <garetxe@gmail.com>
--

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (isJust)
import Data.IORef
import System.Time

import Data.GI.Base

import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as GP
import qualified GI.Gtk as Gtk

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

drawClockBackground :: Bool -> Int -> Int -> Render ()
drawClockBackground quality width height = do
  save
  scale (fromIntegral width) (fromIntegral height)

  save
  setOperator OperatorOver
  when quality drawDropShadow
  drawClockFace quality
  restore

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  drawHourMarks

  restore

drawClockHands :: Bool -> Int -> Int -> Render ()
drawClockHands quality width height = do
  save
  scale (fromIntegral width) (fromIntegral height)

  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  time <- liftIO (getClockTime >>= toCalendarTime)
  let hours   = fromIntegral (if ctHour time >= 12
                                then ctHour time - 12
                                else ctHour time)
      minutes = fromIntegral (ctMin time)
      seconds = fromIntegral (ctSec time)

  drawHourHand quality hours minutes seconds
  drawMinuteHand quality minutes seconds
  drawSecondHand quality seconds

  restore

drawClockForeground :: Bool -> Int -> Int -> Render ()
drawClockForeground quality width height = do
  scale (fromIntegral width) (fromIntegral height)

  save
  translate 0.5 0.5
  scale 0.4 0.4
  setSourceRGB 0.16 0.18 0.19
  setLineWidth (1.5/60)
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  when quality drawInnerShadow
  when quality drawReflection
  drawFrame quality
  restore

drawDropShadow =
  withRadialPattern 0.55 0.55 0.25 0.5 0.5 0.525 $ \pattern -> do
    patternAddColorStopRGBA pattern 0    0     0     0     0.811
    patternAddColorStopRGBA pattern 0.64 0.345 0.345 0.345 0.317
    patternAddColorStopRGBA pattern 0.84 0.713 0.713 0.713 0.137
    patternAddColorStopRGBA pattern 1    1     1     1     0
    patternSetFilter pattern FilterFast
    setSource pattern
    arc 0.5 0.5 (142/150) 0 (pi*2)
    fill

drawClockFace True =
  withLinearPattern 0.5 0 0.5 1 $ \pattern -> do
    patternAddColorStopRGB pattern 0 0.91 0.96 0.93
    patternAddColorStopRGB pattern 1 0.65 0.68 0.68
    patternSetFilter pattern FilterFast
    setSource pattern
    translate 0.5 0.5
    arc 0 0 (60/150) 0 (pi*2)
    fill
drawClockFace False = do
  setSourceRGB 0.78 0.82 0.805
  translate 0.5 0.5
  arc 0 0 (60/150) 0 (pi*2)
  fill

drawHourMarks = do
  save
  forM_ ([1..12] :: [Integer]) $ \_ -> do
    rotate (pi/6)
    moveTo (4.5/6) 0
    lineTo (5.0/6) 0
  stroke
  restore

forM_ = flip mapM_

drawHourHand quality hours minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/6) * hours
         + (pi/360) * minutes
         + (pi/21600) * seconds)

  -- hour hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    moveTo (-2/15 + 0.025) 0.025
    lineTo (7/15 + 0.025) 0.025
    stroke

  -- the hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRGB 0.16 0.18 0.19
  moveTo (-2/15) 0
  lineTo (7/15) 0
  stroke
  restore

drawMinuteHand quality minutes seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate ( (pi/30) * minutes
         + (pi/1800) * seconds)

  -- minute hand's shadow
  when quality $ do
    setLineWidth (1.75/60)
    setOperator OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    moveTo (-16/75 - 0.025) (-0.025)
    lineTo (2/3 - 0.025)    (-0.025)
    stroke

  -- the minute hand itself
  setLineWidth (1/60)
  setOperator OperatorOver
  setSourceRGB 0.16 0.18 0.19
  moveTo (-16/75) 0
  lineTo (2/3) 0
  stroke
  restore

drawSecondHand quality seconds = do
  save
  rotate (-pi/2)
  setLineCap LineCapSquare
  setLineJoin LineJoinMiter
  rotate (seconds * pi/30);

  -- shadow of second hand-part
  when quality $ do
    setOperator  OperatorAtop
    setSourceRGBA 0.16 0.18 0.19 0.125
    setLineWidth  (1.3125 / 60)
    moveTo (-1.5/5 + 0.025) 0.025
    lineTo (3/5 + 0.025) 0.025
    stroke

  -- second hand
  setOperator OperatorOver
  setSourceRGB 0.39 0.58 0.77
  setLineWidth (0.75/60)
  moveTo (-1.5/5) 0
  lineTo (3/5) 0
  stroke

  arc 0 0 (1/20) 0 (pi*2)
  fill
  arc (63/100) 0 (1/35) 0 (pi*2)
  stroke
  setLineWidth  (1/100)
  moveTo  (10/15) 0
  lineTo  (12/15) 0
  stroke
  setSourceRGB  0.31 0.31 0.31
  arc  0 0 (1/25) 0 (pi*2)
  fill
  restore

drawInnerShadow = do
  save
  setOperator OperatorOver
  arc 0 0 (142/150) 0 (pi*2)
  clip
  withRadialPattern 0.3 0.3 0.1 0 0 0.95 $ \pattern -> do
    patternAddColorStopRGBA pattern 0    1     1     1     0
    patternAddColorStopRGBA pattern 0.64 0.713 0.713 0.713 0.137
    patternAddColorStopRGBA pattern 0.84 0.345 0.345 0.345 0.317
    patternAddColorStopRGBA pattern 1    0     0     0     0.811
    patternSetFilter pattern FilterFast
    setSource pattern
    arc 0 0 (142/150) 0 (pi*2)
    fill
  restore

drawReflection = do
  save
  arc 0 0 (142/150) 0 (pi*2)
  clip
  rotate (-75 * pi/180)
  setSourceRGBA 0.87 0.9 0.95 0.25
  moveTo (-1) (-1)
  lineTo 1 (-1)
  lineTo 1 1
  curveTo 1 0.15 (-0.15) (-1) (-1) (-1)
  fill
  moveTo (-1) (-1)
  lineTo (-1) 1
  lineTo 1 1
  curveTo (-0.5) 1 (-1) 0.5 (-1) (-1)
  fill
  restore

drawFrame True = do
  save
  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \pattern -> do
    patternAddColorStopRGB pattern 0   0.4  0.4  0.4
    patternAddColorStopRGB pattern 0.2 0.95 0.95 0.95
    patternSetFilter pattern FilterFast
    setSource pattern
    setLineWidth (10/75)
    arc 0 0 (142/150) 0 (pi*2)
    stroke

  withRadialPattern (-0.1) (-0.1) 0.8 0 0 1.5 $ \pattern -> do
    patternAddColorStopRGB pattern 0   0.9  0.9  0.9
    patternAddColorStopRGB pattern 0.2 0.35 0.35 0.35
    patternSetFilter pattern FilterFast
    setSource pattern
    setLineWidth (10/75)
    arc 0 0 (150/150) 0 (pi*2)
    stroke
  restore
drawFrame False = do
  save
  setSourceRGB 0 0 0
  setLineWidth (10/75)
  arc 0 0 1 0 (pi*2)
  stroke
  restore

initialSize :: Int
initialSize = 256

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  window <- new Gtk.Window
            [ #decorated      := False
            , #resizable      := True
            , #windowPosition := Gtk.WindowPositionCenterAlways
            , #appPaintable   := True
            , #icon           :=> GP.pixbufNewFromFile "cairo-clock-icon.png"
            , #title          := "Haskell-gi Cairo Clock"
            , #defaultWidth   := fromIntegral initialSize
            , #defaultHeight  := fromIntegral initialSize
            ]

  geometry <- new Gdk.Geometry [ #minWidth  := 32
                               , #minHeight := 32
                               , #maxWidth  := 512
                               , #maxHeight := 512 ]
  #setGeometryHints window (Just window) (Just geometry)
                        [Gdk.WindowHintsMinSize, Gdk.WindowHintsMaxSize]

  screen <- window `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual window visual

  on window #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    when (name == Just "Escape") Gtk.mainQuit
    return False

  on window #buttonPressEvent $ \event -> do
    button <- event `get` #button
    time <- event `get` #time
    x <- event `get` #xRoot
    y <- event `get` #yRoot
    case button of
      1 -> do
        #beginMoveDrag window 1 (round x) (round y) time
        return True
      3 -> do
        #beginResizeDrag window Gdk.WindowEdgeSouthEast 3
                         (round x) (round y) time
        return True
      _ -> return False

  GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000
          (#queueDraw window >> return True)

  backgroundRef <- newIORef (Just undefined)
  foregroundRef <- newIORef (Just undefined)

  let redrawStaticLayers = do
        (width32, height32) <- #getSize window
        let (width, height) = (fromIntegral width32, fromIntegral height32)
        background <- createImageSurface FormatARGB32 width height
        foreground <- createImageSurface FormatARGB32 width height
        let clear = do
              save
              setOperator OperatorClear
              paint
              restore
        renderWith background $ do
          clear
          drawClockBackground True width height
        renderWith foreground $ do
          clear
          drawClockForeground True width height
        writeIORef backgroundRef (Just background)
        writeIORef foregroundRef (Just foreground)

  on window #realize redrawStaticLayers

  sizeRef <- newIORef (initialSize, initialSize)
  timeoutHandlerRef <- newIORef Nothing
  on window #configureEvent $ \event -> do
    w <- fromIntegral <$> event `get` #width
    h <- fromIntegral <$> event `get` #height

    size <- readIORef sizeRef
    writeIORef sizeRef (w,h)
    when (size /= (w,h)) $ do

      background <- readIORef backgroundRef
      foreground <- readIORef foregroundRef
      maybe (return ()) surfaceFinish background
      maybe (return ()) surfaceFinish foreground

      writeIORef backgroundRef Nothing
      writeIORef foregroundRef Nothing

      timeoutHandler <- readIORef timeoutHandlerRef
      _ <- maybe (return True) GLib.sourceRemove timeoutHandler

      handler <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT_IDLE 300
                 (do
                   writeIORef timeoutHandlerRef Nothing
                   redrawStaticLayers
                   #queueDraw window
                   return False
                 )
      writeIORef timeoutHandlerRef (Just handler)

    return False

  on window #draw $ \context -> do
    width <- fromIntegral <$> #getAllocatedWidth window
    height <- fromIntegral <$> #getAllocatedHeight window

    background <- readIORef backgroundRef
    foreground <- readIORef foregroundRef

    renderWithContext context $ do
      save
      setOperator OperatorSource
      setSourceRGBA 0 0 0 0
      paint
      restore

      case background of
        Nothing -> drawClockBackground False width height
        Just background -> do
          setSourceSurface background 0 0
          paint

      drawClockHands (isJust background) width height

      case foreground of
        Nothing -> drawClockForeground False width height
        Just foreground -> do
          setSourceSurface foreground 0 0
          paint

    return True

  #showAll window

  Gtk.main
