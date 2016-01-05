{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Lib
import Icons

ex1 = drawIconAndPorts apply0Icon
ex2 = drawIconsAndPortNumbers apply0Icon

main :: IO ()
main = mainWith (ex2 # frame 0.1 # bg black)
