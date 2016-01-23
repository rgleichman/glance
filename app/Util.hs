{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  portToPort,
  iconToPort,
  iconToIcon,
  iconToIconEnds,
  iconHeadToPort,
  iconTailToPort,
  toNames
)where

import Control.Arrow(first)
import Diagrams.Prelude(IsName, toName, Name)

import Types(EdgeEnd(..), Edge(..))

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (first f)

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

-- Edge constructors --
portToPort :: (IsName a, IsName b) => a -> Int -> b -> Int -> Edge
portToPort a b c d = Edge (toName a, Just b, toName c, Just d) noEnds

iconToPort :: (IsName a, IsName b) => a -> b -> Int -> Edge
iconToPort a   c d = Edge (toName a, Nothing, toName c, Just d) noEnds

iconToIcon :: (IsName a, IsName b) => a -> b -> Edge
iconToIcon a   c   = Edge (toName a, Nothing, toName c, Nothing) noEnds


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: (IsName a, IsName b) => a -> EdgeEnd -> b -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge (toName a, Nothing, toName c, Nothing) (b, d)

iconHeadToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
iconHeadToPort a endHead c d = Edge (toName a, Nothing, toName c, Just d) (EndNone, endHead)

iconTailToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
iconTailToPort a endTail c d = Edge (toName a, Nothing, toName c, Just d) (endTail, EndNone)
