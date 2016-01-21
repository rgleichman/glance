{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Types where

import Diagrams.Prelude(Name)

-- TYPES --
-- | A datatype that represents an icon.
-- The BranchIcon is used as a branching point for a line.
-- The TextBoxIcon's data is the text that appears in the text box.
-- The LambdaRegionIcon's data is the number of lambda ports, and the name of it's
-- subdrawing.
data Icon = Apply0Icon | ResultIcon | BranchIcon | TextBoxIcon String | GuardIcon Int
  | LambdaRegionIcon Int Name | Apply0NIcon Int

type Connection = (Name, Maybe Int, Name, Maybe Int)

-- | An Edge has an name of the source icon, and its optional port number,
-- and the name of the destination icon, and its optional port number.
data Edge = Edge {edgeConnection :: Connection, edgeEnds :: (EdgeEnd, EdgeEnd)}

data EdgeEnd = EndAp1Result | EndAp1Arg | EndNone

-- | A drawing is a map from names to Icons, a list of edges,
-- and a map of names to subDrawings
data Drawing = Drawing [(Name, Icon)] [Edge] [(Name, Drawing)]
