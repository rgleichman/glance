module SimplifySyntax where

import qualified Language.Haskell.Exts as Exts

import Translate(qOpToExp)
-- A simplified Haskell syntax tree

-- rhs is now SimpExp

-- A simplified Haskell expression.
data SimpExp l =
  SeName l (Exts.QName l)
  | SeLit l (Exts.Literal l)
  | SeApp l
    (SimpExp l)  -- function
    (SimpExp l)  -- argument
  | SeLambda l [SimpPat l] (SimpExp l)
  | SeLet l [SimpDecl l] (SimpExp l)
  | SeCase l (SimpExp l) [SimpAlt l]
  | SeGuard l [SelectorAndVal l]


data SelectorAndVal l = SelectorAndVal {
  svSelector :: SimpExp l
  , svVal :: SimpExp l
  }

data SimpAlt l = SimpAlt {
  saPat :: SimpPat l
  , saVal :: SimpExp l
  }

data SimpDecl l =
  -- These don't have decl lists, since only lets have decl lists
  SdFunBind l (Exts.Name l) [SimpPat l] (SimpExp l)
  | SdPatBind l (SimpPat l) (SimpExp l)

data SimpPat l =
  SpVar l (Exts.Name l)
  | SpLit l (Exts.Sign l) (Exts.Literal l)
  | SpApp l (Exts.QName l) [SimpPat l]
  | SpAsPat l (Exts.Name l) (SimpPat l)

infixAppToSeApp :: a -> Exts.Exp a -> Exts.QOp a -> Exts.Exp a -> SimpExp a
infixAppToSeApp l e1 op e2 = case op of
  Exts.QVarOp _ (Exts.UnQual _ (Exts.Symbol _ sym)) -> case sym of
    "$" -> hsExpToSimpExp (Exts.App l e1 e2)
    -- TODO
    -- "." -> grNamePortToGrRef
    --        <$> evalFunctionComposition c (e1 : compositionToList e2)
    _ -> defaultCase
  _ -> defaultCase
  where
    defaultCase = hsExpToSimpExp $ Exts.App l (Exts.App l (qOpToExp op) e1) e2

hsExpToSimpExp :: Exts.Exp a -> SimpExp a
hsExpToSimpExp e = case e of
  Exts.Var l n -> SeName l n
  Exts.Con l n -> SeName l n
  Exts.Lit l n -> SeLit l n
  Exts.InfixApp l e1 op e2 -> infixAppToSeApp l e1 op e2
  Exts.App l f arg -> SeApp l (hsExpToSimpExp f) (hsExpToSimpExp arg)
