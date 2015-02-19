module Language.EBNF.FormatLaTeX where

{-
\documentclass{article}
\newcommand{\alt}{\texttt{|}}
\begin{document}
\begin{tabular}{|lrll|}
\hline
\multicolumn{4}{|c|}{Statements} \\
$E$ & ::= & $E$ $+$ $T$ & term \\
    &  \alt & $T$         & \\
$T$ & ::= & $T$ $*$ $F$ & factor \\
    &  \alt & $F$         & \\
\hline
\end{tabular}
\end{document}
-}
