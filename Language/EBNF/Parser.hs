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

  pSpacesOrComments

  vs <- P.many (pVarDef 1)
  return $ Grammar desc vs

pSpacesOrComments :: GP ()
pSpacesOrComments = P.many (pSpacesNonEmpty <|> pInterComment) >> return ()
  where pInterComment = P.try (pCommentBlock >> P.notFollowedBy pVarRef)

-- Maybe preceded by a block of comment lines.
-- E.g. "<Foo> ::= <Bar> | <Baz>? 'qux'"
-- E.g. "<Foo> ::= <Bar> | <Baz>? 'qux'"
--            where v = ...
pVarDef :: Int -> GP Var
pVarDef ind_cl = rule "pVarDef" $ do
   desc <- P.option "" pCommentBlock
   pSpacesNN -- needed for indented comment blocks
             -- e.g. ...
             -- where -- comment here
             --       first def
             --
             -- This will chew up everyting to first definition
   withLoc $ \loc -> do
     cl <- getCl
     when (cl /= ind_cl) $
       fail $ "indentation error (parent has indent " ++ show ind_cl ++ ")"
     vnm <- pVarRef
     pVarDefEq -- ::=
     e <- pExpr ind_cl
     let pWhere = rule "pWhere" $ do
           where_cl <- getCl
           when (where_cl <= ind_cl) $
            fail $ "indentation error where (for " ++ vnm ++ ") must be indented"
           ln <- getLn
           pKeyword "where"
           next_ind <- getCl -- after whitespace following where
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
           when (next_ind <= ind_cl) $
             fail $ "empty where block (starting at line " ++ show ln ++ ")"
           P.many1 (P.try (pVarDef next_ind)) -- need at least one definition
     wes <- P.option [] pWhere

     pSpacesOrComments

     return $! Var desc loc vnm e wes

pVarDefEq :: GP ()
pVarDefEq = pSymbol "::="

-- "<Foo>"
pVarRef :: GP String
pVarRef = pToken $
  P.between (P.char '<') (P.char '>') (P.many1 P.alphaNum)

pExpr :: Int -> GP Expr
-- Top-level grammar expressions
pExpr = pExprAlts

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
  es <- P.many (pExprPostFix min_cl)
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
            ensureIdent min_cl
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

ensureIdent :: Int -> GP ()
ensureIdent min_cl = do
  cl <- getCl
  when (cl <= min_cl) $
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

pCommentLine :: GP String
pCommentLine = body
  where body = do
          pSymbol "--"
          cs <- P.many (P.satisfy (/='\n'))
          P.try (P.char '\n') -- could be EOF
          return (dropWhile isSpace cs)

pSpaces :: GP ()
pSpaces = P.spaces

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
  traceP $ "<== " ++ show a ++ " (from " ++ show lncl ++ ")"
  return a

tracePAt :: (Int,Int) -> String -> GP ()
tracePAt at m =
  trace ("@" ++ show at ++ ": " ++ m) (return ())

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


