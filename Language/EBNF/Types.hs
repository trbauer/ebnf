module Language.EBNF.Types where

import Data.List

import qualified Data.Set as D



data Grammar = Grammar {
    gDesc :: String -- initial comment block that dsecribes the grammar
  , gVars :: [Var]
  } deriving Show

data Var = Var {
    vDesc :: !String
  , vLoc :: !Loc
  , vName :: !String
  , vExpr :: !Expr
  , vWhere :: ![Var]
  } deriving Show


data Expr =
    ExprAlts !Loc ![(Expr,String)] -- E1 | E2 | ...
  | ExprSeq  !Loc ![Expr] -- E1 E2 ...
  | ExprOpt  !Loc !Expr   -- E? (optional)
  | ExprMany !Loc !Expr   -- E* (zero or more)
  | ExprPos  !Loc !Expr   -- E+ (positive number)
  | ExprAs   !Loc !String !Expr -- [<V>]E
  | ExprGrp  !Loc !Expr   -- (E)
  | ExprVar  !Loc !String -- <V>
  | ExprPatt !Loc !String -- @text@
  | ExprLit  !Loc !String -- 'lit'
  | ExprDots !Loc         -- ...
  deriving (Show)


-- Computes the unbound variables (non-terminals) in the grammar
gVarsRefed :: Grammar -> [String]
gVarsRefed = nubOrd . concatMap vVarsRefed . gVars

-- Returns all literals in the grammar
gTermsRefed :: Grammar -> [String]
gTermsRefed = nubOrd . concatMap vTermsRefed . gVars

vVarsRefed :: Var -> [String]
vVarsRefed v = D.toList diff
  where diff = vs `D.difference` loc_bindings
        vs = D.fromList $ eVarsRefed (vExpr v) ++ concatMap vVarsRefed (vWhere v)
        loc_bindings = D.fromList (map vName (vWhere v))

vTermsRefed :: Var -> [String]
vTermsRefed v =
  nubOrd $ eTermsRefed (vExpr v) ++ concatMap vTermsRefed (vWhere v)

nubOrd :: [String] -> [String]
nubOrd = D.toList . D.fromList

eVarsRefed :: Expr -> [String]
eVarsRefed = concatMap eVars . eAllSubExprs
  where eVars :: Expr -> [String]
        eVars (ExprVar _ v) = [v]
        eVars _ = []

eTermsRefed :: Expr -> [String]
eTermsRefed = concatMap eTerms . eAllSubExprs
  where eTerms :: Expr -> [String]
        eTerms (ExprLit _ l) = [l]
        eTerms (ExprPatt _ p) = [p]
        eTerms _ = []

-- direct subexpressions
eSubExprs :: Expr -> [Expr]
eSubExprs (ExprAlts _ as) = map fst as
eSubExprs (ExprSeq _ es) = es
eSubExprs (ExprOpt _ e) = [e]
eSubExprs (ExprMany _ e) = [e]
eSubExprs (ExprPos _ e) = [e]
eSubExprs (ExprAs _ _ e) = [e]
eSubExprs (ExprGrp _ e) = [e]
eSubExprs _ = []

-- transitive closure
eAllSubExprs :: Expr -> [Expr]
eAllSubExprs e = e : concatMap eAllSubExprs (eSubExprs e)




fmtG :: Grammar -> String
fmtG g =
 "Grammar {\n" ++
 "  gDesc = " ++ show (gDesc g) ++ "\n" ++
 ", gVars = " ++ fmtVs "    " (gVars g) ++ "\n" ++
 "}\n"

fmtVs :: String -> [Var] -> String
fmtVs = fmtList fmtV

fmtList :: (String -> a -> String) -> String -> [a] -> String
fmtList f ind [] = "[]"
fmtList f ind (a0:as) = "[\n" ++
  ind ++ "    " ++ f (ind ++ "    ") a0 ++ "\n" ++
  concatMap (\a -> ind ++ "  , " ++ f ("  " ++ ind) a ++ "\n") as ++
  ind ++ "]"

fmtV :: String -> Var -> String
fmtV ind v =
  "Var {\n" ++
  ind ++ "    vDesc = " ++ show (vDesc v) ++ "\n" ++
  ind ++ "  , vLoc = " ++ show (vLoc v) ++ "\n" ++
  ind ++ "  , vName = " ++ show (vName v) ++ "\n" ++
  ind ++ "  , vExpr = " ++ fmtE (ind ++ "  ") (vExpr v) ++ "\n" ++
  ind ++ "  , vWhere = " ++ fmtVs (ind ++ "  ") (vWhere v) ++ "\n" ++
  ind ++ "}"
  where showF nm f = ind ++ ", " ++ nm ++ " = " ++ show (f v)
        where_str = if null (vWhere v) then "[]\n" else "[\n"

fmtE ind (ExprAlts loc as) =
  "ExprAlts " ++ show loc ++ " " ++ fmtList fmtAlt (ind ++ "  ") as
  where fmtAlt ind (e,c) = "(" ++ fmtE ind e ++"," ++ show c ++ ")"
fmtE ind (ExprSeq loc es) = "ExprSeq " ++ show loc ++ " " ++ fmtList fmtE (ind ++ "  ") es
fmtE ind e = show e

type Diag = (Loc,String) -- diagnostic
dLoc :: Diag -> Loc
dLoc = fst
dMessage :: Diag -> String
dMessage = snd

type Loc = (Int,Int) -- line and column
locLn = fst
locCl = snd

fmtLoc :: Loc -> String
fmtLoc l =
  case (locLn l, locCl l) of
    (0,0) -> ""
    (ln,0) -> show ln
    (0,cl) -> show cl
    _ -> show l

fmtDiagsWith :: String -> [Diag] -> String
fmtDiagsWith src = intercalate "\n" . map (fmtDiagWith src)

fmtDiagWith :: String -> Diag -> String
fmtDiagWith src d = fmtDiag d ++ src_line_str
  where lns = lines src
        src_line_str =
          case (locLn (dLoc d), locCl (dLoc d)) of
            (0,_) -> ""
            (_,0) -> ""
            (ln,cl)
              | ln - 1 < length lns - 1 -> ""
              | otherwise -> "\n" ++
                  lns !! (ln - 1) ++ "\n" ++
                  replicate cl ' ' ++ "^"

fmtDiag :: Diag -> String
fmtDiag d = loc_str++ dMessage d
  where loc_str = if null s then "" else s ++ ". "
          where s = fmtLoc (dLoc d)