module Language.EBNF.FormatText where

import Language.EBNF.Types

import Data.List

fmtGrammar :: Grammar -> String
fmtGrammar g = desc ++ intercalate "\n" (map (fmtVar "") (gVars g))
  where desc = if null s then "" else comment s
          where s = gDesc g
                comment = ("\n\n" ++ ) . unlines . map (++"-- ") . lines

fmtVar :: String -> Var -> String
fmtVar ind v = desc_str ++ ind ++ v_str ++ eq_str ++ " " ++ expr_str ++ where_str
  where desc_str
          | null desc = ""
          | otherwise = unlines (map ((ind ++ "-- ")++) (lines desc))
          where desc = vDesc v
        v_str = vName v
        eq_str = " ="
        expr_str =
          case vExpr v of
            (ExprAlts _ [(e,c)])
              -- foo = bar baz qux
              | length short_expr < cols_left -> short_expr
              where short_expr = fmtExpr e ++ comment c
            e@(ExprAlts _ as)
              -- >> [ind]foo = bar baz | qux? biz+ | doz
              | all (null . snd) as &&
                  length short_expr < cols_left -> short_expr
              where short_expr = fmtExpr e
            e@(ExprAlts _ as)
              -- Have comments on some of these or they are too long;
              -- expand alts on to lines
              --
              -- >> [ind]foo =
              -- >> [ind]    bar baz -- comment
              -- >> [ind]  | ...
              | otherwise -> "\n" ++
                  ind ++ "    " ++ fmtExpr e0 ++ comment c0 ++ "\n" ++
                   intercalate "\n" (map fmtAlt (tail as))
              where (e0,c0) = head as
                    fmtAlt (e,c) = ind ++ "  | " ++ fmtExpr e ++ comment c
            e -> fmtExpr e

        cols_left :: Int
        cols_left = 80 - (length ind + length v_str + 1 + length eq_str + 1) -- "var = "

        where_str
          --    [ind]foo =
          --    [ind]    bar baz -- comment
          --    [ind]  | ...
          -- >> [ind]  where ...
          -- >> [ind]
          | null (vWhere v) = ""
          | otherwise       =
            "\n" ++ ind ++ "  " ++ "where\n" ++
            intercalate "\n" (map (fmtVar (ind ++ "    ")) (vWhere v))

        comment "" = ""
        comment s  = " -- " ++ s

fmtExpr :: Expr -> String
fmtExpr (ExprAlts _ as) = intercalate (" | ") (map (fmtExpr . fst) as)
fmtExpr (ExprSeq  _ es) = intercalate " " (map fmtExpr es)
fmtExpr (ExprOpt  _ e)  = fmtExpr e ++ "?"
fmtExpr (ExprMany _ e)  = fmtExpr e ++ "*"
fmtExpr (ExprPos  _ e)  = fmtExpr e ++ "+"
fmtExpr (ExprAs  _ t e) = "[" ++ t ++ "]" ++ fmtExpr e
fmtExpr (ExprGrp  _ e)  = "(" ++ fmtExpr e ++ ")"
fmtExpr (ExprVar  _ v)  = v
fmtExpr (ExprLit  _ l)  = "'" ++ l ++ "'"
fmtExpr (ExprPatt  _ l)  = "@" ++ escP l ++ "@"
  where escP [] = []
        escP ('@':cs) = "\\@" ++ escP cs
        escP (c:cs) = c:escP cs
fmtExpr (ExprDots _)    = "..."
