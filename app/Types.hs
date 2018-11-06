{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ConstraintKinds #-}

module Types (
  NamedIcon(..),
  Icon(..),
  SyntaxNode(..),
  NodeName(..),
  Port(..),
  NameAndPort(..),
  Connection,
  Edge(..),
  EdgeOption(..),
  EdgeEnd(..),
  Drawing(..),
  IDState(..),
  SpecialQDiagram,
  SpecialBackend,
  SpecialNum,
  SgNamedNode(..),
  IngSyntaxGraph,
  LikeApplyFlavor(..),
  CaseOrGuardTag(..),
  Labeled(..)
) where

import Diagrams.Prelude(QDiagram, V2, Any, Renderable, Path, IsName)
import Diagrams.TwoD.Text(Text)

import Control.Applicative(Applicative(..))
import Data.Typeable(Typeable)

data NamedIcon = NamedIcon {niName :: NodeName, niIcon :: Icon}
  deriving (Show, Eq, Ord)

data Labeled a = Labeled {laValue :: a, laLabel :: String}
  deriving (Show, Eq, Ord)

instance Functor Labeled where
  fmap f (Labeled value str) = Labeled (f value) str

instance Applicative Labeled where
  pure x = Labeled x ""
  (Labeled f fStr) <*> (Labeled x xStr) = Labeled (f x) (fStr <> xStr)

-- TYPES --
-- | A datatype that represents an icon.
-- The TextBoxIcon's data is the text that appears in the text box.
data Icon = TextBoxIcon String
  | GuardIcon
    Int  -- Number of alternatives
  | FlatLambdaIcon [String]
  | CaseIcon Int
  | CaseResultIcon
  | BindTextBoxIcon String
  | NestedApply
    LikeApplyFlavor  -- apply or compose
    (Maybe NamedIcon)  -- The function for apply, or the argument for compose
    [Maybe NamedIcon]  -- list of arguments or functions
  | NestedPApp
    (Labeled (Maybe NamedIcon))  -- Data constructor
    [Labeled (Maybe NamedIcon)]  -- Arguments
  | NestedCaseIcon [Maybe NamedIcon]
  | NestedGuardIcon [Maybe NamedIcon]
  deriving (Show, Eq, Ord)

data LikeApplyFlavor = ApplyNodeFlavor | ComposeNodeFlavor
  deriving (Show, Eq, Ord)

data CaseOrGuardTag = CaseTag | GuardTag deriving (Show, Eq, Ord)

-- TODO remove Ints from SyntaxNode data constructors.
data SyntaxNode =
  LikeApplyNode LikeApplyFlavor Int -- Function application, composition, and applying to a composition
  -- NestedApplyNode is only created in GraphAlgorithms, not during translation.
  | NestedApplyNode LikeApplyFlavor Int [(SgNamedNode, Edge)]
  | NestedPatternApplyNode String [Labeled (Maybe SgNamedNode)]
  | NameNode String -- Identifiers or symbols
  | BindNameNode String
  | LiteralNode String -- Literal values like the string "Hello World"
  | FunctionDefNode [String] -- Function definition (ie. lambda expression)
  | GuardNode
    Int  -- Number of alternatives
  | CaseNode Int
  | CaseResultNode -- TODO remove caseResultNode
  | NestedCaseOrGuardNode CaseOrGuardTag Int [(SgNamedNode, Edge)]
  deriving (Show, Eq, Ord)

newtype NodeName = NodeName Int deriving (Typeable, Eq, Ord, Show)
instance IsName NodeName

newtype Port = Port Int deriving (Typeable, Eq, Ord, Show)
instance IsName Port

data NameAndPort = NameAndPort NodeName (Maybe Port) deriving (Show, Eq, Ord)

type Connection = (NameAndPort, NameAndPort)

-- TODO Consider removing EdgeOption and EdgeEnd since they are unused.
data EdgeOption = EdgeInPattern deriving (Show, Eq, Ord)

data EdgeEnd = EndNone deriving (Show, Eq, Ord)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge {edgeOptions::[EdgeOption], edgeEnds :: (EdgeEnd, EdgeEnd), edgeConnection :: Connection}
  deriving (Show, Eq, Ord)

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [NamedIcon] [Edge] deriving (Show, Eq)

data SgNamedNode = SgNamedNode NodeName SyntaxNode deriving (Ord, Eq, Show)

-- | IDState is an Abstract Data Type that is used as a state whose value is a unique id.
newtype IDState = IDState Int deriving (Eq, Show)

type SpecialNum n = (Floating n, RealFrac n, RealFloat n, Typeable n, Show n, Enum n)

-- Note that SpecialBackend is a constraint synonym, not a type synonym.
type SpecialBackend b n = (SpecialNum n, Renderable (Path V2 n) b, Renderable (Text n) b)

type SpecialQDiagram b n = QDiagram b V2 n Any

type IngSyntaxGraph gr = gr SgNamedNode Edge
