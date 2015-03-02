module Language.EBNF.FormatHTML where

import Language.EBNF.Types
import Language.EBNF.FormatText

import Data.List

fmtGrammarHTML :: Grammar -> String
fmtGrammarHTML g =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" ++
    "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" ++
    "  <head>\n" ++
    "    <title>Grammar</title>\n" ++
    "    <style type=\"text/css\">\n" ++
    "      b {font-family:sans-serif}\n" ++
    "    </style>\n" ++
    "  </head>\n" ++
    "  <body>\n" ++
    fmtGrm "  " g ++
    "  </body>\n" ++
    "</html>"
  where fmtGrm :: String -> Grammar -> String
        fmtGrm ind g = concatMap fmtRootVar (gVars g)

        --- | desc...                                |
        --- | VAR        | =   | value    | desc   |
        --  |            |   |   | value    | desc   |
        --  | ... where  |                           |
        --  |       WHV  | =   | value    | desc   |
        --  |            |   |   | value    | desc   |
        --  | ...
        fmtRootVar :: Var -> String
        fmtRootVar v =
--          "  <table style=\"width:" ++ show table_width_px ++ "px\">\n" ++
          "  <table style=\"width:" ++ show table_width_px ++ "px\">\n" ++
          "  <tr><td colspan=\"4\">&nbsp;</td></tr>\n" ++
          fmtVar "  " 1 v ++
          "  </table>\n"

        table_width_px = 1024

        fmtVar :: String -> Int -> Var -> String
        fmtVar ind ilvl v =
            v_desc_tr ++
            ind ++ "<tr>\n" ++
            ind ++ "  <td style=\"width:128px;padding-left:" ++
                    show ((ilvl - 1) * 48) ++ "px\">" ++
                      "<a name=\"" ++ vName v ++ "\" />" ++
                      fmtVarName (ilvl == 1) (vName v) ++ "</td>\n" ++
                    "  " ++ equals_td ++ short_rule_string_tds ++
            ind ++ "</tr>\n" ++
            long_rule_string ++
            ind ++ where_clause
          where (short_rule_string_tds,long_rule_string) =
                  case vExpr v of
                    e@(ExprAlts _ (a:as))
                      | all (null . snd) (a:as) && length (fmtExpr e) < 64 ->
                        ("<td>" ++ fmtHtmlExpr e ++ "</td>","")
                      | otherwise ->
                        (fmtAltTDs a, fmtLongAlts as)
                    e -> (ind ++ fmtAltTDs (e,""),"")

                v_desc_tr
                  | null dstr = ""
                  | otherwise = ind ++ "<tr><td style=\"font-family:sans-serif;padding-left:" ++
                                            show (48*(ilvl - 1)) ++ "px\" colspan=\"4\">" ++
                                            desc dstr ++ "</td></tr>\n"
                  where dstr = vDesc v

                fmtLongAlts :: [(Expr,String)] -> String
                fmtLongAlts as =concatMap fmtLongAltLine as
                  where fmtLongAltLine :: (Expr,String) -> String
                        fmtLongAltLine a =
                           ind ++ "<tr><td></td>" ++ alt_bar_td ++ "\n" ++ fmtAltTDs a ++ "</tr>"

                fmtAltTDs :: (Expr,String) -> String
                fmtAltTDs (e,d) = ind ++ "<td>" ++ fmtHtmlExpr e ++ "</td><td style=\"font-family:sans-serif\">" ++ desc d ++ "</td>"

                where_clause
                  | null v_wheres = ""
                  | otherwise =
                    ind ++ "<tr><td style=\"padding-left:" ++ show (ilvl * 32) ++ "px\" colspan=\"4\">" ++ meta "where" ++ "</td></tr>\n" ++
                      concatMap (fmtVar ind (ilvl + 1)) v_wheres
                v_wheres = vWhere v

                equals_td  = ind ++ "<td style=\"width:32px\">" ++ metaNoEsc "&rarr;" ++ "</td>"
                alt_bar_td = ind ++ "<td style=\"width:32px\">" ++ meta "|" ++ "</td>"

        fmtHtmlExpr :: Expr -> String
        fmtHtmlExpr (ExprAlts _ es) = intercalate (meta (" | ")) $ map (fmtHtmlExpr . fst) es
        fmtHtmlExpr (ExprSeq  _ es) = intercalate " " $ map fmtHtmlExpr es
        fmtHtmlExpr (ExprOpt  _ e)  = fmtHtmlExpr e ++ meta "?"
        fmtHtmlExpr (ExprMany _ e)  = fmtHtmlExpr e ++ meta "*"
        fmtHtmlExpr (ExprPos  _ e)  = fmtHtmlExpr e ++ meta "+"
        fmtHtmlExpr (ExprAs  _ t e) = meta "[<" ++ esc t ++ meta ">]" ++ fmtHtmlExpr e
        fmtHtmlExpr (ExprGrp  _ e)  = meta "(" ++ fmtHtmlExpr e ++ meta ")"
        fmtHtmlExpr (ExprVar  _ v)  = fmtVarRef v
        fmtHtmlExpr (ExprPatt _ p)  = term (esc p)
        fmtHtmlExpr (ExprLit  _ l)  = term (esc l)
        fmtHtmlExpr (ExprDots _)    = "..."

        desc :: String -> String
        desc = gray . escDesc
        meta :: String -> String
        meta = metaNoEsc . esc
        metaNoEsc :: String -> String
        metaNoEsc = red
        term "" = ""
        term str = "<span style=\"font-size:16px;font-family:monospace;color:rgb(0,0,192)\">" ++ str ++ "</span>"
        red "" = ""
        red  str = "<span style=\"color:red\">" ++ str ++ "</span>"
        gray "" = ""
        gray str = "<span style=\"color:gray\">" ++ str ++  "</span>"

        fmtVarRef :: String -> String
        fmtVarRef = fmtVarName False

        fmtVarName :: Bool -> String -> String
        fmtVarName z vref =
            "<span style=\"font-size:16px;font-style:italic" ++ bold_str ++ "\">" ++ esc vref ++ "</span>"
          where bold_str = if z then ";font-weight:bold" else ""

        -- escapes HTML
        esc :: String -> String
        esc ('<':cs) = "&lt;" ++ esc cs
        esc ('>':cs) = "&gt;" ++ esc cs
        esc ('&':cs) = "&amp;" ++ esc cs
        esc ('"':cs) = "&quot;" ++ esc cs
        esc ('\'':cs) = "&apos;" ++ esc cs
        esc (c:cs) = c:esc cs
        esc [] = []

        -- escapes HTML with some handling of embedded preformatting statatements
        -- E.g. @foo@ preformats foo
        escDesc :: String -> String
        escDesc (d:cs)
          | d `elem` "\'@" = "<span style=\"font-family:monospace\">" ++ addLitSpan cs
          where addLitSpan [] = " [ERROR: unclosed literal escape with \\" ++ esc [d] ++ "]" -- unclosed @ or '
                addLitSpan ('\\':c:cs)
                  | c == d    = esc [c] ++ addLitSpan cs -- escaped
                addLitSpan (c:cs)
                  | c == d    = "</span>" ++ escDesc cs
                  | otherwise = esc [c] ++ addLitSpan cs
        escDesc (c:cs) = esc [c] ++ escDesc cs
        escDesc [] = []
