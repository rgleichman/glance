{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, ConstraintKinds #-}

module Types (
  Icon(..),
  SyntaxNode(..),
  NameAndPort(..),
  Connection,
  Edge(..),
  EdgeOption(..),
  EdgeEnd(..),
  Drawing(..),
  IDState,
  SpecialQDiagram,
  SpecialBackend,
  SgNamedNode,
  IngSyntaxGraph,
  initialIdState,
  getId,
  sgNamedNodeToSyntaxNode
) where

import Diagrams.Prelude(Name, QDiagram, V2, Any, Renderable, Path)
import Diagrams.TwoD.Text(Text)
import Control.Monad.State(State, state)

-- TYPES --
-- | A datatype that represents an icon.
-- The BranchIcon is used as a branching point for a line.
-- The TextBoxIcon's data is the text that appears in the text box.
-- The LambdaRegionIcon's data is the number of lambda ports, and the name of it's
-- subdrawing.
data Icon = ResultIcon | BranchIcon | TextBoxIcon String | GuardIcon Int
  | FlatLambdaIcon Int | ApplyAIcon Int
  | PAppIcon Int String | CaseIcon Int | CaseResultIcon
  | BindTextBoxIcon String
  -- TODO: NestedApply should have the type NestedApply (Maybe (Name, Icon)) [Maybe (Name, Icon)]
  | NestedApply [Maybe (Name, Icon)]
  | NestedPApp [Maybe (Name, Icon)]
  deriving (Show, Eq, Ord)

-- TODO remove Ints from SyntaxNode data constructors.
-- TODO Add NestedApplyNode, and NestedPatternApplyNode
data SyntaxNode =
  ApplyNode Int-- Function application
  | NestedApplyNode Int [(SgNamedNode, Edge)]
  | PatternApplyNode String Int -- Destructors as used in patterns
  | NestedPatternApplyNode String Int [(SgNamedNode, Edge)]
  | NameNode String -- Identifiers or symbols
  | BindNameNode String
  | LiteralNode String -- Literal values like the string "Hello World"
  | FunctionDefNode Int-- Function definition (ie. lambda expression)
  | GuardNode Int
  | CaseNode Int
  | BranchNode -- TODO remove BranchNode
  | CaseResultNode -- TODO remove caseResultNode
  deriving (Show, Eq, Ord)

data NameAndPort = NameAndPort Name (Maybe Int) deriving (Show, Eq, Ord)

type Connection = (NameAndPort, NameAndPort)

data EdgeOption = EdgeInPattern deriving (Show, Eq, Ord)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge {edgeOptions::[EdgeOption], edgeEnds :: (EdgeEnd, EdgeEnd), edgeConnection :: Connection}
  deriving (Show, Eq, Ord)

data EdgeEnd = EndAp1Result | EndAp1Arg | EndNone deriving (Show, Eq, Ord)

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [(Name, Icon)] [Edge] deriving (Show, Eq)

-- | IDState is an Abstract Data Type that is used as a state whose value is a unique id.
newtype IDState = IDState Int deriving (Eq, Show)

-- Note that SpecialBackend is a constraint synonym, not a type synonym.
type SpecialBackend b = (Renderable (Path V2 Double) b, Renderable (Text Double) b)

type SpecialQDiagram b = QDiagram b V2 Double Any

type SgNamedNode = (Name, SyntaxNode)
type IngSyntaxGraph gr = gr SgNamedNode Edge

sgNamedNodeToSyntaxNode :: SgNamedNode -> SyntaxNode
sgNamedNodeToSyntaxNode = snd

initialIdState :: IDState
initialIdState = IDState 0

getId :: State IDState Int
getId = state incrementer where
  incrementer (IDState x) = (x, IDState checkedIncrement) where
    xPlusOne = x + 1
    checkedIncrement = if xPlusOne > x
      then xPlusOne
      else error "getId: the ID state has overflowed."
