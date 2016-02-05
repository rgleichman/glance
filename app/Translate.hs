{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude((<>))

import Language.Haskell.Exts(Decl(..), parseDecl,
  Name(..), Pat(..), Rhs(..), Exp(..), QName(..), fromParseResult) --(parseFile, parse, ParseResult, Module)

import Types(Icon, Edge(..), Drawing(..), NameAndPort)
import Util(toNames, noEnds, nameAndPort, justName)
import Icons(Icon(..))

data IconGraph = IconGraph [(DIA.Name, Icon)] [Edge]

instance DIA.Semigroup IconGraph where
  (IconGraph x1 y1) <> (IconGraph x2 y2) = IconGraph (x1 <> x2) (y1 <> y2)

instance Monoid IconGraph where
  mempty = IconGraph [] []
  mappend = (<>)

--instance

nameToString :: Language.Haskell.Exts.Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

evalPattern :: Pat -> String
evalPattern p = case p of
  PVar n -> nameToString n
  -- TODO other cases

-- evalMatch :: Match -> (String, [String])
-- evalMatch (Match _ name patterns _ _ _) = res where
--   patternStrings = map evalPattern patterns
--   matchName = nameToString name
--   res = (matchName, patternStrings)

evalQName :: QName -> (IconGraph, NameAndPort)
evalQName (UnQual n) = (graph, justName nameString) where
  nameString = nameToString n
  graph = IconGraph [(DIA.toName nameString, TextBoxIcon nameString)] []
-- TODO other cases

evalApp :: Int -> (IconGraph, NameAndPort) -> (IconGraph, NameAndPort) -> (IconGraph, NameAndPort)
evalApp uniqueInt (funGr, funNamePort) (argGr, argNamePort) =
  (newGraph <> funGr <> argGr, nameAndPort applyIconName 2)
    where
      newGraph = IconGraph icons edges
      -- TODO figure out unique names for the apply icons
      applyIconString = "app0" ++ show uniqueInt
      applyIconName = DIA.toName applyIconString
      icons = [(applyIconName, Apply0Icon)]
      edges = [
        Edge (funNamePort, nameAndPort applyIconName 0) noEnds,
        Edge (argNamePort, nameAndPort applyIconName 1) noEnds
        ]

evalExp :: Int -> Exp -> (IconGraph, NameAndPort)
evalExp uniqueInt x = case x of
  Var n -> evalQName n
  App exp1 exp2 -> evalApp uniqueInt (evalExp (uniqueInt + 1) exp1) (evalExp 0 exp2)
  -- TODO other cases

evalRhs :: Rhs -> (IconGraph, NameAndPort)
evalRhs (UnGuardedRhs e) = evalExp 0 e
evalRhs (GuardedRhss _) = error "GuardedRhss not implemented"

evalPatBind :: Decl -> IconGraph
evalPatBind (PatBind _ pat rhs binds) = graph <> rhsGraph where
  patName = evalPattern pat
  (rhsGraph, rhsNamePort) = evalRhs rhs
  icons = toNames [
    (patName, TextBoxIcon patName)
    --(rhsName, TextBoxIcon rhsName)
    ]
  edges = [
    -- TODO use port here
     Edge (justName patName, rhsNamePort) noEnds
     ]
  graph = IconGraph icons edges

evalDecl :: Decl -> IconGraph
evalDecl d = case d of
  pat@PatBind{} -> evalPatBind pat
  -- TODO other cases

translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  IconGraph icons edges = evalDecl decl
  drawing = Drawing icons edges []
