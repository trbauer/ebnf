module Language.EBNF.EBNF(
    module Language.EBNF.Types
  , parseGrammar, parseGrammar'

  , eVarsRefed, eTermsRefed, eSubExprs, eAllSubExprs
  ) where

import Language.EBNF.FormatText
import Language.EBNF.Parser
import Language.EBNF.Types

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
