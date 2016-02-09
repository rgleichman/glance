{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Translate(
  translateString
) where

import qualified Diagrams.Prelude as DIA
import Diagrams.Prelude((<>))

import Language.Haskell.Exts(Decl(..), parseDecl,
  Name(..), Pat(..), Rhs(..), Exp(..), QName(..), fromParseResult, Match(..)) --(parseFile, parse, ParseResult, Module)
import Control.Monad.State(State, evalState)
import Data.List(elemIndex)

import Types(Icon, Edge(..), Drawing(..), NameAndPort(..), IDState,
  initialIdState, getId)
import Util(toNames, noEnds, nameAndPort, justName, fromMaybeError)
import Icons(Icon(..))

data IconGraph = IconGraph [(DIA.Name, Icon)] [Edge] [(DIA.Name, Drawing)] [(String, NameAndPort)]

type EvalContext = [String]

instance DIA.Semigroup IconGraph where
  (IconGraph icons1 edges1 subDrawings1 context1) <> (IconGraph icons2 edges2 subDrawings2 context2) =
    IconGraph (icons1 <> icons2) (edges1 <> edges2) (subDrawings1 <> subDrawings2) (context1 <> context2)

instance Monoid IconGraph where
  mempty = IconGraph [] [] [] []
  mappend = (<>)

nameToString :: Language.Haskell.Exts.Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

evalPattern :: Pat -> String
evalPattern p = case p of
  PVar n -> nameToString n
  -- TODO other cases

evalQName :: QName -> EvalContext -> Either String (IconGraph, NameAndPort)
evalQName (UnQual n) context = result where
  nameString = nameToString n
  graph = IconGraph [(DIA.toName nameString, TextBoxIcon nameString)] [] [] []
  result = if nameString `elem` context
    then Left nameString
    else Right (graph, justName nameString)
-- TODO other cases

evalApp :: Exp -> Exp -> EvalContext -> State IDState (Either String (IconGraph, NameAndPort))
evalApp exp1 exp2 c = do -- State Monad
  funVal <- evalExp exp1 c
  argVal <- evalExp exp2 c
  newId <- getId
  let
    getGraph port val = case val of
      Left s -> (mempty, mempty, [(s, port)])
      Right (gr, p) -> (gr, [Edge (p, port) noEnds], mempty)

    functionPort = nameAndPort applyIconName 0
    (funGr, funEdges, funBoundVars) = getGraph functionPort funVal
    argumentPort = nameAndPort applyIconName 1
    (argGr, argEdges, argBoundVars) = getGraph argumentPort argVal
    newGraph = IconGraph icons (funEdges <> argEdges) mempty (funBoundVars <> argBoundVars)
    applyIconString = "app0" ++ show newId
    applyIconName = DIA.toName applyIconString
    icons = [(applyIconName, Apply0Icon)]
  pure $ Right (newGraph <> funGr <> argGr, nameAndPort applyIconName 2)

evalExp :: Exp  -> EvalContext -> State IDState (Either String (IconGraph, NameAndPort))
evalExp x c = case x of
  Var n -> pure $ evalQName n c
  App exp1 exp2 -> evalApp exp1 exp2 c
  Paren e -> evalExp e c
  -- TODO other cases

-- | First argument is the right hand side.
-- The second arugement is a list of strings that are bound in the environment.
evalRhs :: Rhs -> EvalContext -> (IconGraph, NameAndPort)
evalRhs (UnGuardedRhs e) scope = case evalState (evalExp e scope) initialIdState of
  Left _ -> error "rhs result expression is a bound var."
  Right x -> x
-- TODO implement other cases.
--evalRhs (GuardedRhss _) _ = error "GuardedRhss not implemented"

evalPatBind :: Decl -> IconGraph
evalPatBind (PatBind _ pat rhs _) = graph <> rhsGraph where
  patName = evalPattern pat
  (rhsGraph, rhsNamePort) = evalRhs rhs []
  icons = toNames [
    (patName, TextBoxIcon patName)
    --(rhsName, TextBoxIcon rhsName)
    ]
  edges = [
    -- TODO use port here
     Edge (justName patName, rhsNamePort) noEnds
     ]
  graph = IconGraph icons edges [] []

iconGraphToDrawing :: IconGraph -> Drawing
iconGraphToDrawing (IconGraph icons edges subDrawings _) = Drawing icons edges subDrawings

evalMatch :: Match -> IconGraph
evalMatch (Match _ name patterns _ rhs _) = drawing
  where
    -- TODO unique names for lambdaName and resultName
    lambdaName = "lam"
    nameString = nameToString name
    lambdaPorts = map (nameAndPort lambdaName) [0,1..]
    patternStringMap =
      (nameString, justName lambdaName) : zip (map evalPattern patterns) lambdaPorts

    patternStrings = map fst patternStringMap
    numParameters = length patterns
    (rhsGraph, rhsResult) = evalRhs rhs patternStrings
    resultName = "res"
    rhsNewIcons = toNames [(resultName, ResultIcon)]
    rhsNewEdges = [Edge (rhsResult, justName resultName) noEnds]
    rhsGraphWithResult = rhsGraph <> IconGraph rhsNewIcons rhsNewEdges [] []
    rhsDrawing = iconGraphToDrawing rhsGraphWithResult
    rhsDrawingName = DIA.toName "rhsDraw"
    icons = toNames [
      (lambdaName, LambdaRegionIcon numParameters rhsDrawingName),
      (nameString, TextBoxIcon nameString)
      ]
    (IconGraph _ _ _ boundVars) = rhsGraph

    qualifyNameAndPort :: String -> NameAndPort -> NameAndPort
    qualifyNameAndPort s (NameAndPort n p) = NameAndPort (s DIA..> n) p

    boundVarsToEdge (s, np) =
      Edge (source, qualifyNameAndPort lambdaName np) noEnds
      where
        source = fromMaybeError "evalMatch: bound var not found" $ lookup s patternStringMap

    externalEdges = [Edge (justName nameString, justName lambdaName) noEnds]
    internalEdges = boundVarsToEdge <$> filter (\(s, _) -> s `elem` patternStrings) boundVars
    drawing = IconGraph icons (externalEdges <> internalEdges) [(rhsDrawingName, rhsDrawing)] []


evalMatches :: [Match] -> IconGraph
evalMatches [] = IconGraph [] [] [] []
evalMatches [match] = evalMatch match
-- TODO turn more than one match into a case expression.

evalDecl :: Decl -> Drawing
evalDecl d = iconGraphToDrawing $ case d of
  pat@PatBind{} -> evalPatBind pat
  FunBind matches -> evalMatches matches
  -- TODO other cases

translateString :: String -> (Drawing, Decl)
translateString s = (drawing, decl) where
  parseResult = parseDecl s -- :: ParseResult Module
  decl = fromParseResult parseResult
  drawing = evalDecl decl
