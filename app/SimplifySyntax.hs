module SimplifySyntax (
  SimpExp(..)
  , SelectorAndVal(..)
  , SimpAlt(..)
  , SimpDecl(..)
  , SimpPat(..)
  , stringToSimpDecl
  , qOpToExp
  , qNameToString
  , nameToString
  , customParseDecl
  , hsDeclToSimpDecl
  ) where

import Data.List(foldl')

import qualified Language.Haskell.Exts as Exts

import TranslateCore(nTupleString, nListString)
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
  deriving (Show, Eq)

data SelectorAndVal l = SelectorAndVal {
  svSelector :: SimpExp l
  , svVal :: SimpExp l
  }
  deriving (Show, Eq)

data SimpAlt l = SimpAlt {
  saPat :: SimpPat l
  , saVal :: SimpExp l
  }
  deriving (Show, Eq)

data SimpDecl l =
  -- These don't have decl lists, since only lets have decl lists
  SdPatBind l (SimpPat l) (SimpExp l)
  deriving (Show, Eq)

data SimpPat l =
  SpVar l (Exts.Name l)
  | SpLit l (Exts.Sign l) (Exts.Literal l)
  | SpApp l (Exts.QName l) [SimpPat l]
  | SpAsPat l (Exts.Name l) (SimpPat l)
  | SpWildCard l
  deriving (Show, Eq)

-- Helper functions

strToQName :: l -> String -> Exts.QName l
strToQName l = Exts.UnQual l . Exts.Ident l

makeVarExp :: l -> String -> Exts.Exp l
makeVarExp l = Exts.Var l . strToQName l

qOpToExp :: Exts.QOp l -> Exts.Exp l
qOpToExp (Exts.QVarOp l n) = Exts.Var l n
qOpToExp (Exts.QConOp l n) = Exts.Con l n

nameToString :: Exts.Name l -> String
nameToString (Exts.Ident _ s) = s
nameToString (Exts.Symbol _ s) = s

-- TODO refactor qNameToString
qNameToString :: Show l => Exts.QName l -> String
qNameToString (Exts.Qual _ (Exts.ModuleName _ modName) name)
  = modName ++ "." ++ nameToString name
qNameToString (Exts.UnQual _ name) = nameToString name
qNameToString (Exts.Special _ (Exts.UnitCon _)) = "()"
qNameToString (Exts.Special _ (Exts.ListCon _)) = "[]"
qNameToString (Exts.Special _ (Exts.FunCon _)) = "(->)"
qNameToString (Exts.Special _ (Exts.TupleCon _ _ n)) = nTupleString n
qNameToString (Exts.Special _ (Exts.Cons _)) = "(:)"
-- unboxed singleton tuple constructor
qNameToString (Exts.Special _ (Exts.UnboxedSingleCon _)) = "(# #)"
qNameToString q = error $ "Unsupported syntax in qNameToSrting: " <> show q

--

hsPatToSimpPat :: Show a => Exts.Pat a -> SimpPat a
hsPatToSimpPat p = case p of
  Exts.PVar l n -> SpVar l n
  Exts.PLit l sign lit -> SpLit l sign lit
  Exts.PInfixApp l p1 qName p2 -> hsPatToSimpPat (Exts.PApp l qName [p1, p2])
  Exts.PApp l name patts -> SpApp l name (fmap hsPatToSimpPat patts)
  Exts.PTuple l _ patts -> SpApp
                           l
                           ((strToQName l . nTupleString . length) patts)
                           (fmap hsPatToSimpPat patts)
  Exts.PParen _ pat -> hsPatToSimpPat pat
  Exts.PAsPat l name pat -> SpAsPat l name (hsPatToSimpPat pat)
  Exts.PWildCard l -> SpWildCard l
  Exts.PList l patts -> SpApp
                        l
                        ((strToQName l . nListString . length) patts)
                        (fmap hsPatToSimpPat patts)
  _ -> error $  "Unsupported syntax in hsPatToSimpPat: " <> show p

whereToLet :: Show a => a -> Exts.Rhs a -> Maybe (Exts.Binds a) -> SimpExp a
whereToLet l rhs maybeBinds = val
  where
    rhsExp = hsRhsToExp rhs
    val = case maybeBinds of
      Nothing -> rhsExp
      Just binds -> SeLet l (hsBindsToDecls binds) rhsExp

matchToSimpDecl :: Show a => Exts.Match a -> SimpDecl a
matchToSimpDecl (Exts.Match l name patterns rhs maybeWhereBinds)
  = SdPatBind
    l
    (SpVar l name)
    (SeLambda l
     (fmap hsPatToSimpPat patterns)
     (whereToLet l rhs maybeWhereBinds))
matchToSimpDecl m = error $ "Unsupported syntax in matchToSimpDecl: " <> show m

-- Only used by matchesToCase
matchToAlt :: Show l => Exts.Match l -> Exts.Alt l
matchToAlt (Exts.Match l _ mtaPats rhs binds)
  = Exts.Alt l altPattern rhs binds
  where
    altPattern = case mtaPats of
      [onePat] -> onePat
      _ -> Exts.PTuple l Exts.Boxed mtaPats
matchToAlt match = error $ "Unsupported syntax in matchToAlt: " <> show match

-- TODO Refactor matchesToCase
matchesToCase :: Show l => Exts.Match l -> [Exts.Match l] -> Exts.Match l
matchesToCase match [] = match
matchesToCase firstMatch@(Exts.Match srcLoc funName pats _ _) restOfMatches = match
  where
    -- There is a special case in Icons.hs/makeLabelledPort to exclude " tempvar"
    -- TODO use a data constructor for the special case instead of using string
    -- matching for tempvars.
    tempStrings = fmap (\x -> " tempvar" ++ show x) [0..(length pats - 1)]
    tempPats = fmap (Exts.PVar srcLoc . Exts.Ident srcLoc) tempStrings
    tempVars = fmap (makeVarExp srcLoc) tempStrings
    tuple = Exts.Tuple srcLoc Exts.Boxed tempVars
    caseExp = case tempVars of
      [oneTempVar] -> Exts.Case srcLoc oneTempVar alts
      _ -> Exts.Case srcLoc tuple alts
    rhs = Exts.UnGuardedRhs srcLoc caseExp
    match = Exts.Match srcLoc funName tempPats rhs Nothing
    allMatches = firstMatch:restOfMatches
    alts = fmap matchToAlt allMatches
matchesToCase firstMatch _
  = error $ "Unsupported syntax in matchesToCase: " <> show firstMatch

matchesToFunBind :: Show a => a -> [Exts.Match a] -> SimpDecl a
matchesToFunBind l matches = case matches of
  [] -> error $ "Empty matches in matchesToFunBind. Label is :" <> show l
  (m : ms) -> matchToSimpDecl (matchesToCase m ms)

hsDeclToSimpDecl :: Show a => Exts.Decl a -> SimpDecl a
hsDeclToSimpDecl decl = case decl of
  Exts.FunBind l matches -> matchesToFunBind l matches
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

simplifyExp :: SimpExp l -> SimpExp l
simplifyExp e = case e of
  -- Reduce applications of function compositions (e.g. (f . g) x -> f (g x))
  SeApp l2 (SeApp l1 (SeApp _ (SeName _ ".") f1) f2) arg
    -> SeApp l1 f1 $ simplifyExp (SeApp l2 f2 arg)
  SeApp l (SeApp _ (SeName _ "$") exp1) exp2
    -> SeApp l exp1 exp2
  SeApp l1 (SeName l2 "<$>") arg
    -> SeApp l1 (SeName l2 "fmap") arg
  x -> x

deListifyApp :: Show l => l -> Exts.Exp l -> [Exts.Exp l] -> Exts.Exp l
deListifyApp l = foldl' (Exts.App l)

hsExpToSimpExp :: Show a => Exts.Exp a -> SimpExp a
hsExpToSimpExp x = simplifyExp $ case x of
  Exts.Var l n -> SeName l (qNameToString n)
  Exts.Con l n -> SeName l (qNameToString n)
  Exts.Lit l n -> SeLit l n
  Exts.InfixApp l e1 op e2 ->
    hsExpToSimpExp $ Exts.App l (Exts.App l (qOpToExp op) e1) e2
  Exts.App l f arg -> SeApp l (hsExpToSimpExp f) (hsExpToSimpExp arg)
  Exts.Lambda l patterns e
    -> SeLambda l (fmap hsPatToSimpPat patterns) (hsExpToSimpExp e)
  Exts.Let l bs e -> SeLet l (hsBindsToDecls bs) (hsExpToSimpExp e)
  Exts.If l e1 e2 e3
    -> ifToGuard l (hsExpToSimpExp e1) (hsExpToSimpExp e2) (hsExpToSimpExp e3)
  Exts.Case l e alts -> SeCase l (hsExpToSimpExp e) (fmap hsAltToSimpAlt alts)
  Exts.Paren _ e -> hsExpToSimpExp e
  Exts.List l exprs -> hsExpToSimpExp $ deListifyApp
                       l
                       (makeVarExp l $ nListString $ length exprs)
                       exprs
  Exts.Tuple l _ exprs -> hsExpToSimpExp $ deListifyApp
                       l
                       (makeVarExp l $ nTupleString $ length exprs)
                       exprs
  _ -> error $ "Unsupported syntax in hsExpToSimpExp: " ++ show x

-- Parsing

customParseMode :: Exts.ParseMode
customParseMode = Exts.defaultParseMode
  {Exts.extensions =
   [Exts.EnableExtension Exts.MultiParamTypeClasses,
    Exts.EnableExtension Exts.FlexibleContexts,
    Exts.EnableExtension Exts.TupleSections
   ]
  }

customParseDecl :: String -> Exts.Decl Exts.SrcSpanInfo
customParseDecl = Exts.fromParseResult . Exts.parseDeclWithMode customParseMode

stringToSimpDecl :: String -> SimpDecl Exts.SrcSpanInfo
stringToSimpDecl = hsDeclToSimpDecl . customParseDecl
