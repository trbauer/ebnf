module Language.EBNF.Types where

import Data.List

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

type Loc = (Int,Int) -- line and column

data Expr =
    ExprAlts !Loc ![(Expr,String)] -- E1 | E2 | ...
  | ExprSeq  !Loc ![Expr] -- E1 E2 ...
  | ExprOpt  !Loc !Expr   -- E? (optional)
  | ExprMany !Loc !Expr   -- E* (zero or more)
  | ExprPos  !Loc !Expr   -- E+ (positive number)
  | ExprGrp  !Loc !Expr   -- (E)
  | ExprVar  !Loc !String -- <V>
  | ExprLit  !Loc !String -- 'lit'
  | ExprDots !Loc         -- ...
  deriving (Show)


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
-- fmtE ind (ExprOpt loc e) = "ExprOpt " ++ show loc ++ " " ++ fmtE ind e
-- fmtE ind (ExprMany loc e) = "ExprMany " ++ show loc ++ " " ++ fmtE ind e
-- fmtE ind (ExprPos loc e) = "ExprPos " ++ show loc ++ " " ++ fmtE ind e
-- fmtE ind (ExprGrp loc e) = "ExprGrp " ++ show loc ++ " " ++ fmtE ind e
-- fmtE ind (ExprVar loc v) = "ExprVar " ++ show loc ++ " " ++ show v
-- fmtE ind (ExprLit loc l) = "ExprLit " ++ show loc ++ " " ++ show l
-- fmtE ind (ExprDots loc) = "ExprDots " ++ show loc
fmtE ind e = show e
