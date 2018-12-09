module SimplifySyntax where

import qualified Language.Haskell.Exts as Exts

import Translate(qOpToExp, qNameToString)
-- A simplified Haskell syntax tree

-- rhs is now SimpExp

-- A simplified Haskell expression.
data SimpExp l =
  SeName l String
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

infixAppToSeApp :: Show a =>
  a -> Exts.Exp a -> Exts.QOp a -> Exts.Exp a -> SimpExp a
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

hsPatToSimpPat :: Exts.Pat a -> SimpPat a
hsPatToSimpPat = undefined

whereToLet :: Show a => a -> Exts.Rhs a -> Maybe (Exts.Binds a) -> SimpExp a
whereToLet l rhs maybeBinds = val
  where
    rhsExp = hsRhsToExp rhs
    val = case maybeBinds of
      Nothing -> rhsExp
      Just binds -> SeLet l (hsBindsToDecls binds) rhsExp

matchesToLambda :: a -> [Exts.Match a] -> SimpDecl a
matchesToLambda = undefined

hsDeclToSimpDecl :: Show a => Exts.Decl a -> SimpDecl a
hsDeclToSimpDecl decl = case decl of
  Exts.FunBind l matches -> matchesToLambda l matches
  Exts.PatBind l pat rhs maybeBinds -> SdPatBind l (hsPatToSimpPat pat) expr
    where
      expr = whereToLet l rhs maybeBinds
  -- TODO Exts.TypeSig
  _ -> error $ "Unsupported syntax in hsDeclToSimpDecl: " ++ show decl

hsBindsToDecls :: Show a => Exts.Binds a -> [SimpDecl a]
hsBindsToDecls binds = case binds of
  Exts.BDecls _ decls -> fmap hsDeclToSimpDecl decls
  _ -> error $ "Unsupported syntax in hsBindsToDecls: " ++ show binds

guardedRhsToSelectorAndVal :: Show a => Exts.GuardedRhs a -> SelectorAndVal a
guardedRhsToSelectorAndVal rhs = case rhs of
  Exts.GuardedRhs _ [s] valExp -> SelectorAndVal{svSelector=stmtToExp s
                                                , svVal=hsExpToSimpExp valExp}
  _ -> error $ "Unsupported syntax in guardedRhsToSelectorAndVal: " ++ show rhs
  where
    stmtToExp stmt = case stmt of
      Exts.Qualifier _ e -> hsExpToSimpExp e
      _ -> error
           $ "Unsupported syntax in stmtToExp: " ++ show stmt

hsRhsToExp :: Show a => Exts.Rhs a -> SimpExp a
hsRhsToExp rhs = case rhs of
  Exts.UnGuardedRhs _ e -> hsExpToSimpExp e
  Exts.GuardedRhss l rhss
    -> SeGuard l (fmap guardedRhsToSelectorAndVal rhss)

hsAltToSimpAlt :: Show a => Exts.Alt a -> SimpAlt a
hsAltToSimpAlt (Exts.Alt l pat rhs maybeBinds)
  = SimpAlt{saPat=hsPatToSimpPat pat, saVal=whereToLet l rhs maybeBinds}

ifToGuard :: a -> SimpExp a -> SimpExp a -> SimpExp a -> SimpExp a
ifToGuard l e1 e2 e3
  = SeGuard l [SelectorAndVal{svSelector=e1, svVal=e2}
              , SelectorAndVal{svSelector=otherwiseExp, svVal=e3}]
  where
    otherwiseExp = SeName l "otherwise"

hsExpToSimpExp :: Show a => Exts.Exp a -> SimpExp a
hsExpToSimpExp x = case x of
  Exts.Var l n -> SeName l (qNameToString n)
  Exts.Con l n -> SeName l (qNameToString n)
  Exts.Lit l n -> SeLit l n
  Exts.InfixApp l e1 op e2 -> infixAppToSeApp l e1 op e2
  Exts.App l f arg -> SeApp l (hsExpToSimpExp f) (hsExpToSimpExp arg)
  Exts.Lambda l patterns e
    -> SeLambda l (fmap hsPatToSimpPat patterns) (hsExpToSimpExp e)
  Exts.Let l bs e -> SeLet l (hsBindsToDecls bs) (hsExpToSimpExp e)
  Exts.If l e1 e2 e3
    -> ifToGuard l (hsExpToSimpExp e1) (hsExpToSimpExp e2) (hsExpToSimpExp e3)
  Exts.Case l e alts -> SeCase l (hsExpToSimpExp e) (fmap hsAltToSimpAlt alts)
  Exts.Paren _ e -> hsExpToSimpExp e
  _ -> error $ "Unsupported syntax in hsExpToSimpExp: " ++ show x
