import qualified Data.Map as M

type TermSet = M.Map String String

addTerminals :: [String] -> TermSet
addTerminals = foldl' addTerminal M.empty

addTerminal :: TermSet -> String -> TermSet
addTerminal ts term = tryStr 1 base
  where base = fmtLiteralName term

        try :: Int -> TermSet
        try k = tryStr k (base ++ show k)

        tryStr :: Int -> String -> TermSet
        tryStr k sym
          | sym `M.member` ts = try (k + 1)
          | otherwise = M.insert sym term ts

fmtLiteralName :: String -> String
fmtLiteralName = concatMap fmtLiteralChar

fmtLiteralChar :: Char -> String
fmtLiteralChar '(' = "LPAREN"
fmtLiteralChar ')' = "RPAREN"
fmtLiteralChar '[' = "LBRACK"
fmtLiteralChar ']' = "RBRACK"
fmtLiteralChar '<' = "LANGLE"
fmtLiteralChar '>' = "RANGLE"
fmtLiteralChar '{' = "LCURLY"
fmtLiteralChar '}' = "RCURLY"
fmtLiteralChar 'X' = "XXXXXXX"
fmtLiteralChar '~' = "TILDE"
fmtLiteralChar '`' = "BACKTICK"
fmtLiteralChar '!' = "BANG"
fmtLiteralChar '@' = "AT"
fmtLiteralChar '#' = "HASH"
fmtLiteralChar '$' = "DOLLAR"
fmtLiteralChar '%' = "PERCENT"
fmtLiteralChar '^' = "CIRCUMFLEX"
fmtLiteralChar '&' = "AMP"
fmtLiteralChar '*' = "STAR"
fmtLiteralChar '-' = "DASH"
fmtLiteralChar '_' = "UNDERBAR"
fmtLiteralChar '+' = "PLUS"
fmtLiteralChar '=' = "EQ"
fmtLiteralChar '\b' = "BACKSPACE"

fmtLiteralChar '\t' = "TAB"
fmtLiteralChar '|' = "PIPE"
fmtLiteralChar '\\' = "BACKSLASH"
fmtLiteralChar ':' = "COLON"
fmtLiteralChar ';' = "SEMI"
fmtLiteralChar '\'' = "QUOTE"
fmtLiteralChar '\"' = "DQUOTE"
fmtLiteralChar '\n' = "NEWLINE"
fmtLiteralChar '\f' = "FORMFEED"
fmtLiteralChar '\r' = "CR"

fmtLiteralChar ',' = "COMMA"
fmtLiteralChar '.' = "DOT"
fmtLiteralChar '?' = "QUESTION"
fmtLiteralChar '/' = "SLASH"

fmtLiteralChar c = [toUpper c]