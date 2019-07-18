{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  printSelf,
  iconToPort,
  makeSimpleEdge,
  nameAndPort,
  justName,
  fromMaybeError,
  maybeBoolToBool,
  nodeNameToInt,
  customRenderSVG,
  namedToTuple,
  tupleToNamed
  ) where

import Diagrams.Backend.SVG(renderSVG', Options(..), SVG)
import qualified Diagrams.Prelude as Dia
import Graphics.Svg.Attributes(bindAttr, AttrTag(..))

import Data.Maybe(fromMaybe)
import Data.Text(pack)
import Data.Typeable(Typeable)
import qualified Debug.Trace

import Types(Edge(..), NameAndPort(..), Connection, NodeName(..), Port
            , Named(..))


makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge []

nameAndPort :: NodeName -> Port -> NameAndPort
nameAndPort n p = NameAndPort n (Just p)

justName :: NodeName -> NameAndPort
justName n = NameAndPort n Nothing

-- BEGIN Edge constructors --
iconToPort :: NodeName -> NodeName -> Port -> Edge
iconToPort a   c d = makeSimpleEdge (justName a, nameAndPort c d)

-- END Edge constructors --

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or

nodeNameToInt :: NodeName -> Int
nodeNameToInt (NodeName x) = x

namedToTuple :: Named a -> (NodeName, a)
namedToTuple (Named x y) = (x, y)

tupleToNamed :: (NodeName, a) -> Named a
tupleToNamed (x, y) = Named x y

customRenderSVG :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> Dia.SizeSpec Dia.V2 n
  -> Dia.QDiagram SVG Dia.V2 n Dia.Any
  -> IO ()
customRenderSVG outputFilename size = renderSVG' outputFilename svgOptions where
  -- This xml:space attribute preserves the whitespace in the svg text.
  attributes = [bindAttr XmlSpace_ (pack "preserve")]
  -- TODO Look at the source of renderSVG to see what the 3rd argument to
  -- SVGOptions should be
  svgOptions = SVGOptions size Nothing (pack "") attributes True
