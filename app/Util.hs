{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  portToPort,
  iconToPort,
  iconToIcon,
  iconToIconEnds,
  --iconHeadToPort,
  iconTailToPort,
  toNames,
  noEnds,
  nameAndPort,
  justName,
  fromMaybeError
)where

import Control.Arrow(first)
import Diagrams.Prelude(IsName, toName, Name)
import Data.Maybe(fromMaybe)

import Types(EdgeEnd(..), Edge(..), NameAndPort(..))

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f = map (first f)

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

nameAndPort :: IsName a => a -> Int -> NameAndPort
nameAndPort n p = NameAndPort (toName n) (Just p)

justName :: IsName a => a -> NameAndPort
justName n = NameAndPort (toName n) Nothing

-- Edge constructors --
portToPort :: (IsName a, IsName b) => a -> Int -> b -> Int -> Edge
portToPort a b c d = Edge (nameAndPort a b, nameAndPort c d) noEnds

iconToPort :: (IsName a, IsName b) => a -> b -> Int -> Edge
iconToPort a   c d = Edge (justName a, nameAndPort c d) noEnds

iconToIcon :: (IsName a, IsName b) => a -> b -> Edge
iconToIcon a   c   = Edge (justName a, justName c) noEnds


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: (IsName a, IsName b) => a -> EdgeEnd -> b -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge (justName a, justName c) (b, d)

-- iconHeadToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
-- iconHeadToPort a endHead c d = Edge (justName a, nameAndPort c d) (EndNone, endHead)

iconTailToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
iconTailToPort a endTail c d = Edge (justName a, nameAndPort c d) (endTail, EndNone)

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)
