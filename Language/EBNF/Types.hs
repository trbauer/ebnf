module Language.EBNF.Types where

data Grammar = Grammar {
    gDesc :: String -- initial comment block that dsecribes the grammar
  , gVars :: [Var]
  } deriving Show

data Var = Var {
    vDesc :: !String
  , vLoc :: !Int
  , vName :: !String
  , vExpr :: !Expr
  , vWhere :: ![Var]
  } deriving Show

type Loc = Int
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



