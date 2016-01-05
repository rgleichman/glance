{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Lib
import Icons

ex1 = drawIconAndPorts apply0Icon
ex2 = drawIconsAndPortNumbers apply0Icon

applyDia = iconDia apply0Icon
--apply0A = "A" .>> applyDia
apply0A = applyDia # nameDiagram "A"
apply0B = applyDia # nameDiagram "B"
ex3 = atPoints (map p2 [(0,0), (3,0)]) [apply0A, apply0B]
fromAtoB = ex3 # connectPorts "A" (PortName 0) "B" (PortName 2)
ex4 = apply0A ||| textBox "hello world" === textBox "1" === textBox "gpq" === textBox ['A'..'Z']
ex5 = textBox "baz" # named "baz"||| hrule 1 ||| fromAtoB ||| hrule 1 ||| textBox "foo" # named "foo" === vrule 1 === textBox "bar" # named "bar"
ex6 = ex5 # connectIconToPort "baz" "A" (PortName 2) # connectIconToPort "foo" "B" (PortName 0)
  # connectIconToPort "bar" "B" (PortName 3)

main :: IO ()
main = mainWith (ex6 # frame 0.1 # bg black)
