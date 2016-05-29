{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  portToPort,
  iconToPort,
  iconToIcon,
  iconToIconEnds,
  --iconHeadToPort,
  iconTailToPort,
  toNames,
  makeSimpleEdge,
  noEnds,
  nameAndPort,
  justName,
  fromMaybeError,
  mapFst,
  printSelf,
  eitherToMaybes
)where

import Control.Arrow(first)
import Diagrams.Prelude(IsName, toName, Name)
import Data.Maybe(fromMaybe)
import qualified Debug.Trace

import Types(EdgeEnd(..), Edge(..), NameAndPort(..), Connection)

mapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
mapFst f = fmap (first f)

toNames :: (IsName a) => [(a, b)] -> [(Name, b)]
toNames = mapFst toName

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge [] noEnds

nameAndPort :: IsName a => a -> Int -> NameAndPort
nameAndPort n p = NameAndPort (toName n) (Just p)

justName :: IsName a => a -> NameAndPort
justName n = NameAndPort (toName n) Nothing

-- Edge constructors --
portToPort :: (IsName a, IsName b) => a -> Int -> b -> Int -> Edge
portToPort a b c d = makeSimpleEdge (nameAndPort a b, nameAndPort c d)

iconToPort :: (IsName a, IsName b) => a -> b -> Int -> Edge
iconToPort a   c d = makeSimpleEdge (justName a, nameAndPort c d)

iconToIcon :: (IsName a, IsName b) => a -> b -> Edge
iconToIcon a   c   = makeSimpleEdge (justName a, justName c)


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: (IsName a, IsName b) => a -> EdgeEnd -> b -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge [] (b, d) (justName a, justName c)

-- iconHeadToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
-- iconHeadToPort a endHead c d = Edge (justName a, nameAndPort c d) (EndNone, endHead)

iconTailToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
iconTailToPort a endTail c d = Edge [] (endTail, EndNone) (justName a, nameAndPort c d)

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

eitherToMaybes :: Either a b -> (Maybe a, Maybe b)
eitherToMaybes (Left x) = (Just x, Nothing)
eitherToMaybes (Right y) = (Nothing, Just y)
