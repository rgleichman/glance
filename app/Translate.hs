{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString
) where

import qualified Diagrams.Prelude as DIA

import Language.Haskell.Exts(Match(..), Decl(..), parseDecl, ParseResult(..),
  Name(..), Pat(..), Rhs(..), Exp(..), QName(..), fromParseResult) --(parseFile, parse, ParseResult, Module)

import Types(Icon, Edge, Drawing(..))
import Util(toNames, iconToIcon)
import Icons(Icon(..))

nameToString (Ident s) = s
nameToString (Symbol s) = s

evalPattern p = case p of
  PVar n -> nameToString n
  -- TODO other cases

evalMatch (Match _ name patterns _ _ _) = res where
  patternStrings = map evalPattern patterns
  matchName = nameToString name
  res = (matchName, patternStrings)

evalQName (UnQual n) = nameToString n
-- TODO other cases

evalVar x = case x of
  Var n -> evalQName n
  -- TODO other cases

evalRhs (UnGuardedRhs e) = evalVar e
evalRhs (GuardedRhss _) = error "GuardedRhss not implemented"

evalPatBind :: Decl -> ([(DIA.Name, Icon)], [Edge])
evalPatBind (PatBind _ pat rhs binds) = (icons, edges) where
  patName = evalPattern pat
  rhsName = evalRhs rhs
  icons = toNames [
    (patName, TextBoxIcon patName),
    (rhsName, TextBoxIcon rhsName)
    ]
  edges = [
    iconToIcon patName rhsName
    ]

evalDecl d = case d of
  pat@PatBind{} -> evalPatBind pat
  -- TODO other cases

translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  (icons, edges) = evalDecl decl
  drawing = Drawing icons edges []
