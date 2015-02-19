module Language.EBNF.EBNF(
    module Language.EBNF.Types
  , parseGrammar, parseGrammar'

  , gVarsRefed, gTermsRefed
  , vVarsRefed, vTermsRefed
  , eVarsRefed, eTermsRefed
  , eSubExprs, eAllSubExprs
  ) where

import Language.EBNF.FormatText
import Language.EBNF.Parser
import Language.EBNF.Types

import qualified Data.Set as D


gVarsRefed :: Grammar -> [String]
gVarsRefed = nubOrd . concatMap vVarsRefed . gVars

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
        eTerms _ = []

-- direct subexpressions
eSubExprs :: Expr -> [Expr]
eSubExprs (ExprAlts _ as) = map fst as
eSubExprs (ExprSeq _ es) = es
eSubExprs (ExprOpt _ e) = [e]
eSubExprs (ExprMany _ e) = [e]
eSubExprs (ExprPos _ e) = [e]
eSubExprs (ExprGrp _ e) = [e]
eSubExprs _ = []

-- transitive closure
eAllSubExprs :: Expr -> [Expr]
eAllSubExprs e = e : concatMap eAllSubExprs (eSubExprs e)
