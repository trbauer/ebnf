module Language.EBNF.Parser where

import Language.EBNF.Types

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Int
import Data.List
import Debug.Trace
import qualified Data.ByteString as S
import qualified Text.Parsec.Char       as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Error      as P
import qualified Text.Parsec.Pos        as P
import qualified Text.Parsec.Prim       as P

parseGrammar :: String -> Either String Grammar
parseGrammar inp =
  case P.runParser (pGrammar <* P.eof) () "" inp of
    Left perr -> Left $ show (ln,col) ++ ". " ++ msg ++ "\n" ++ lnstr ++ "\n" ++ replicate col ' ' ++ "^"
      where col = P.sourceColumn (P.errorPos perr) - 1
            ln = P.sourceLine (P.errorPos perr)
            lnstr = lines inp !! (ln - 1)
            msg = intercalate "; "  (tail (lines (show perr)))
            -- msg = intercalate "; " (map show es)
            --   where es = P.errorMessages perr
    Right g -> Right g

parseGrammar' :: String -> Grammar
parseGrammar' inp =
  case parseGrammar inp of
    Left err -> error $ "parseGrammar': " ++ err
    Right g -> g

-- parses a grammar
pGrammar :: GP Grammar
pGrammar = do
  pSpaces
  desc <- P.option "" (P.try (pCommentBlock <* pSpacesNonEmpty))
  pSpacesOrCommentsNoDesc 1
  vs <- P.many (pVarDef 1)
  return $ Grammar desc vs

option :: Show a => String -> a -> GP a -> GP a
option s a = rule ("P.option." ++ s) . P.option a

pSpacesOrCommentsNoDesc :: Int -> GP ()
pSpacesOrCommentsNoDesc ind = P.many (pSpacesNonEmpty <|> pNonDescComment ind) >> return ()

pNonDescComment :: Int -> GP ()
pNonDescComment ind = P.try $ do
  cl <- getCl
  c <- pCommentBlockInd ind
  when (cl == ind) $ do
    P.notFollowedBy $ do
      pVarRef
      pSpacesNN
      pVarDefEq
  pSpaces

pSpacesOrComments :: GP () -- ignores block descriptors
pSpacesOrComments = P.many (pSpacesNonEmpty <|> (pCommentLine >> return ())) >> return ()



-- Parses a variable definition.  Includes an optional preceding
-- comment block and the preceding annotation comment.
-- It is indentation-sensitive and the enclosing scope's indent is passed.
--
--  -- comment about foo
--  foo ::= ...
--    where -- comment about bar
--          bar ::= ...
--           -- not a comment about baz (wrong indent)
--          baz ::= ...
--         -- not a comment about qux (wrong indent)
--          qux ::= ...
--
--   -- not a comment about foo since one column too far over
--  foo ::= ...
--    where
--
--
pVarDef :: Int -> GP Var
pVarDef parent_ind = rule "pVarDef" $ do
  desc <- P.option "" (pCommentBlockInd parent_ind)
  pSpacesNN -- needed for indented comment blocks
             -- e.g. ...
             -- where -- comment here
             --       first-def ::= ...
             --
             -- This will chew up everything to first definition
  withLoc $ \vdef_loc@(vdef_ln,vdef_cl) -> do
    when (parent_ind /= vdef_cl) $
      fail $ "indentation error (enclosing scope has indent " ++ show parent_ind ++ ")"
    vnm <- rule "pVarRef" pVarRef
    pSpacesOrComments
    pVarDefEq -- ::=
    pSpacesOrComments
    e <- rule "pExpr" $ pExpr parent_ind
    let pWhere :: GP [Var]
        pWhere = rule "pWhere" $ do
          where_kw_cl <- getCl
          when (parent_ind >= where_kw_cl) $
            fail $ "indentation error where (for " ++ vnm ++ ") must be indented"
          where_ln <- getLn
          pKeyword "where"
          where_vdef_ind <- getCl -- after whitespace following where
          -- could be next line, or could be right after the word
          -- indentation for all definitions belonging to this where
          -- must fit at this column
          -- could be next line, that's okay
          --  where
          --   foo = bar
          --     where
          --      bar = baz
          --      ^ <== OKAY
          -- BUT:
          --  where
          --     foo = bar
          --       where
          --   bar = baz
          --   ^ <=== NOT OKAY
          --     ^ <=== since <= parent
          -- however, it can't be less than the parent
          when (where_vdef_ind <= parent_ind) $
            fail $ "empty where block (starting at line " ++ show where_ln ++ ")"
-- FIXME: need to scope the try deeper this backs off too much?
--        could do a lookahead to the follow set of pVarDef
          P.many1 (P.try (pVarDef where_vdef_ind)) -- need at least one definition
    wes <- P.option [] pWhere
    rule "pVarDef.pSpacesOrComments" (pSpacesOrCommentsNoDesc parent_ind)
    return $! Var desc vdef_loc vnm e wes

pVarDefEq :: GP ()
pVarDefEq = pSymbol "::="

-- "<Foo>"
pVarRef :: GP String
pVarRef = pToken $
  P.between (P.char '<') (P.char '>') (P.many1 P.alphaNum)

pExpr :: Int -> GP Expr
-- Top-level grammar expressions
pExpr = label "EBNF expression" . pExprAlts

-- E1 | E2 | ...
pExprAlts :: Int -> GP Expr
pExprAlts min_cl = rule "pExprAlts" $ withLoc pAlts
  where pAlts loc = (ExprAlts loc $!) <$> P.sepBy1 pAlt (pSymbol "|")
        pAlt = do
          ln1 <- getLn
          es <- pExprSeq min_cl
          ln2 <- getLn
          comm <-
            if ln1 == ln2
              then P.option "" (pCommentLine <* pSpaces)
              else return ""
          return (es,comm)

-- E1 E2 ...
pExprSeq :: Int -> GP Expr
pExprSeq min_cl = rule "pExprSeq" $ withLoc $ \loc -> do
  es <- P.many (P.try (pExprPostFix min_cl))
  case es of
    [e] -> return e
    _ -> return $! ExprSeq loc es

-- E* or E? ...
pExprPostFix :: Int -> GP Expr
pExprPostFix min_cl = rule "pExprPostFix" $ withLoc $ \loc -> pExprPrim min_cl >>= pPostFixesOp loc
  where pPostFixesOp loc e =
                pOp ExprOpt "?"
            <|> pOp ExprMany "*"
            <|> pOp ExprPos "+"
            <|> return e
          where pOp cons op = pSymbol op >> (return $! cons loc e)

--   | ExprGrp  !Expr   -- (E)
--  | ExprLit  !String -- 'lit'
--  | ExprDots         -- ...
pExprPrim :: Int -> GP Expr
pExprPrim min_cl = rule "pExprPrim" $ withLoc $ \loc -> pPrim loc
  where pPrim loc = do
            ensureIdent min_cl (>)
            pExprGrp <|> pExprVar <|> pExprLit <|> pExprDots
          where pExprGrp = do
                  pSymbol "("
                  e <- pExpr min_cl
                  pSymbol ")"
                  return $ ExprGrp loc e
                pExprVar  = do
                  vref <- pVarRef
                  P.notFollowedBy pVarDefEq
                  return $! ExprVar loc vref
                pExprLit  = (ExprLit loc $!) <$> pLiteral
                pExprDots = pSymbol "..." >> return (ExprDots loc)

-- 'foo' or '\''
pLiteral :: GP String
pLiteral = do
  let pEscChar c = P.char c >> return c
      pEsc = P.try (P.char '\\' >> (pEscChar '\\' <|> pEscChar '\''))
  P.char '\''
  cs <- P.many1 $ pEsc <|> P.satisfy (/='\'')
  P.char '\''
  P.spaces
  return cs

ensureIdent :: Int -> (Int -> Int -> Bool) -> GP ()
ensureIdent min_cl op = do
  cl <- getCl
  when (min_cl `op` cl) $
    fail $ "indentation error (from parent level " ++ show min_cl ++ ")"

-----------------------------------------------------
-- COMBINATORS
pKeyword :: String -> GP ()
pKeyword str = P.try $ pExactly str <* (P.notFollowedBy P.alphaNum >> pSpaces)

pExactly :: String -> GP ()
pExactly str = sequence (map P.char str) >> return ()

-- Parses a literal string (consumes spaces after)
pSymbol :: String -> GP ()
pSymbol str = pExactly str <* pSpaces

-- Parses spaces around a token
pToken :: GP a -> GP a
pToken p = p <* pSpaces

pCommentBlock :: GP String
pCommentBlock = label "comment block" $ intercalate "\n" <$> P.many1 pCommentLine

-- A uniformly indented comment block
pCommentBlockInd :: Int -> GP String
pCommentBlockInd ind = label ("comment block ("++ show ind ++ ")") $ intercalate "\n" <$> P.many1 pCommentLineInd
  where pCommentLineInd = do
          cl <- getCl
          when (cl /= ind) $
            fail $ "wrong indent for block comment (parent has " ++ show ind ++ ")"
          pCommentLine <* pSpaces

pCommentLine :: GP String
pCommentLine = body
  where body = do
          pSymbol "--"
          cs <- P.many (P.satisfy (/='\n'))
          P.try (pChar '\n' <|> pEof) -- could be EOF
          return (dropWhile isSpace cs)

pSpaces :: GP ()
pSpaces = P.spaces

pChar :: Char -> GP ()
pChar c = P.char c >> return ()

pEof :: GP ()
pEof = P.eof

pSpacesNonEmpty :: GP ()
pSpacesNonEmpty = P.many1 (P.oneOf " \n\t\r") >> return ()

pSpacesNN :: GP ()
pSpacesNN = P.many (P.oneOf " \t") >> return ()

getLoc :: GP Loc
getLoc = getLnCl

getLn :: GP Int
getLn = P.sourceLine <$> P.getPosition

getCl :: GP Int
getCl = P.sourceColumn <$> P.getPosition

getLnCl :: GP (Int,Int)
getLnCl = mkPair <$> P.getPosition
  where mkPair = P.sourceLine &&& P.sourceColumn

withLoc :: (Loc -> GP a) -> GP a
withLoc f = getLoc >>= f

label :: String -> GP a -> GP a
label = flip (P.<?>)

--------------- DEBUG --------------

rule :: Show a => String -> GP a -> GP a
-- rule = ruleVerb
rule _ = id

ruleVerb :: Show a => String -> GP a -> GP a
ruleVerb s r = do
  lncl <- getLnCl
  traceP $ "==> " ++ s
  a <- r
  traceP $ "<== " ++ s ++ ": " ++ show a ++ " (from " ++ show lncl ++ ")"
  return a

tracePAt :: (Int,Int) -> String -> GP ()
tracePAt at m = do
  inp <- P.getInput
  trace ("TRACE@" ++ show at ++ " with LA[" ++ (take 8 inp) ++ "...]: " ++ m) (return ())

traceP :: String -> GP ()
traceP m = getLnCl >>= flip tracePAt m

testP :: Show a => GP a -> String -> IO ()
testP p inp =
    case P.runParser par () "" inp of
      Left perr -> print perr
      Right (a,lo) -> print a >> putStrLn ("leftover: " ++ show lo)
  where par = do
          a <- p
          lo <- P.getInput
          return (a,lo)

testP' :: Show a => GP a -> String -> IO ()
testP' p inp =
  case P.runParser (p <* P.eof) () "" inp of
    Left perr -> print perr
    Right a -> print a

type GP = P.Parsec String ()


