{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  printSelf,
  iconToPort,
  makeSimpleEdge,
  noEnds,
  nameAndPort,
  justName,
  fromMaybeError,
  maybeBoolToBool,
  mapNodeInNamedNode,
  sgNamedNodeToSyntaxNode,
  nodeNameToInt,
  customRenderSVG,
  namedIconToTuple,
  tupleToNamedIcon
  ) where

import Diagrams.Backend.SVG(renderSVG', Options(..), SVG)
import qualified Diagrams.Prelude as Dia
import Graphics.Svg.Attributes(bindAttr, AttrTag(..))

import Data.Maybe(fromMaybe)
import Data.Text(pack)
import Data.Typeable(Typeable)
import qualified Debug.Trace

import Types(EdgeEnd(..), Edge(..), NameAndPort(..), Connection, NodeName(..)
            , Port, SyntaxNode, SgNamedNode(..), NamedIcon(..), Icon(..))

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge [] noEnds

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

mapNodeInNamedNode :: (SyntaxNode -> Icon) -> SgNamedNode -> NamedIcon
mapNodeInNamedNode f (SgNamedNode name node) = NamedIcon name (f node)

sgNamedNodeToSyntaxNode :: SgNamedNode -> SyntaxNode
sgNamedNodeToSyntaxNode (SgNamedNode _ n) = n

nodeNameToInt :: NodeName -> Int
nodeNameToInt (NodeName x) = x

namedIconToTuple :: NamedIcon -> (NodeName, Icon)
namedIconToTuple (NamedIcon x y) = (x, y)

tupleToNamedIcon :: (NodeName, Icon) -> NamedIcon
tupleToNamedIcon (x, y) = NamedIcon x y

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
