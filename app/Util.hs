{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Util (
  portToPort,
  iconToPort,
  iconToIcon,
  iconToIconEnds,
  --iconHeadToPort,
  iconTailToPort,
  makeSimpleEdge,
  noEnds,
  nameAndPort,
  justName,
  fromMaybeError,
  mapFst,
  printSelf,
  eitherToMaybes,
  maybeBoolToBool,
  mapNodeInNamedNode,
  sgNamedNodeToSyntaxNode,
  nodeNameToInt,
  customRenderSVG
)where

import Diagrams.Backend.SVG(renderSVG', Options(..), SVG)
import Graphics.Svg.Attributes(bindAttr, AttrTag(..))
import qualified Diagrams.Prelude as Dia

import Control.Arrow(first)
-- import Diagrams.Prelude(IsName, toName, Name)
import Data.Maybe(fromMaybe)
import qualified Debug.Trace
import Data.Text(pack)
import Data.Typeable(Typeable)

import Types(EdgeEnd(..), Edge(..), NameAndPort(..), Connection, NodeName(..), Port,
             SyntaxNode, SgNamedNode(..))

mapFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
mapFst f = fmap (first f)

noEnds :: (EdgeEnd, EdgeEnd)
noEnds = (EndNone, EndNone)

makeSimpleEdge :: Connection -> Edge
makeSimpleEdge = Edge [] noEnds

nameAndPort :: NodeName -> Port -> NameAndPort
nameAndPort n p = NameAndPort n (Just p)

justName :: NodeName -> NameAndPort
justName n = NameAndPort n Nothing

-- BEGIN Edge constructors --
portToPort :: NodeName -> Port -> NodeName -> Port -> Edge
portToPort a b c d = makeSimpleEdge (nameAndPort a b, nameAndPort c d)

iconToPort :: NodeName -> NodeName -> Port -> Edge
iconToPort a   c d = makeSimpleEdge (justName a, nameAndPort c d)

iconToIcon :: NodeName -> NodeName -> Edge
iconToIcon a   c   = makeSimpleEdge (justName a, justName c)


-- If there are gaps between the arrow and the icon, try switching the first two arguments
-- with the last two arguments
iconToIconEnds :: NodeName -> EdgeEnd -> NodeName -> EdgeEnd -> Edge
iconToIconEnds a b c d = Edge [] (b, d) (justName a, justName c)

-- iconHeadToPort :: (IsName a, IsName b) => a -> EdgeEnd -> b -> Int -> Edge
-- iconHeadToPort a endHead c d = Edge (justName a, nameAndPort c d) (EndNone, endHead)

iconTailToPort :: NodeName -> EdgeEnd -> NodeName -> Port -> Edge
iconTailToPort a endTail c d = Edge [] (endTail, EndNone) (justName a, nameAndPort c d)

-- END Edge constructors --

fromMaybeError :: String -> Maybe a -> a
fromMaybeError s = fromMaybe (error s)

printSelf :: (Show a) => a -> a
printSelf a = Debug.Trace.trace (show a ++ "\n\n") a

eitherToMaybes :: Either a b -> (Maybe a, Maybe b)
eitherToMaybes (Left x) = (Just x, Nothing)
eitherToMaybes (Right y) = (Nothing, Just y)

-- | (Just True) = True, Nothing = False
maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool = or

mapNodeInNamedNode :: (SyntaxNode -> a) -> SgNamedNode -> (NodeName, a)
mapNodeInNamedNode f (SgNamedNode name node) = (name, f node)

sgNamedNodeToSyntaxNode :: SgNamedNode -> SyntaxNode
sgNamedNodeToSyntaxNode (SgNamedNode _ n) = n

nodeNameToInt :: NodeName -> Int
nodeNameToInt (NodeName x) = x


customRenderSVG :: (Typeable n, Show n, RealFloat n) =>
  FilePath
  -> Dia.SizeSpec Dia.V2 n
  -> Dia.QDiagram SVG Dia.V2 n Dia.Any
  -> IO ()
customRenderSVG outputFilename size = renderSVG' outputFilename svgOptions where
  -- This xml:space attribute preserves the whitespace in the svg text.
  attributes = [bindAttr XmlSpace_ (pack "preserve")]
  -- TODO Look at the source of renderSVG to see what the 3rd argument to SVGOptions should be
  svgOptions = SVGOptions size Nothing (pack "") attributes True
