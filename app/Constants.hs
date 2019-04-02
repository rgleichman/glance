{-# LANGUAGE PatternSynonyms #-}
module Constants
  ( pattern InputPortConst
  , pattern ResultPortConst
  ) where

import Types(Port(..))

pattern InputPortConst :: Port
pattern InputPortConst = Port 0

pattern ResultPortConst :: Port
pattern ResultPortConst = Port 1
