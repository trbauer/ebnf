
import Language.EBNF.EBNF
import Language.EBNF.Parser
import qualified Language.EBNF.FormatText as T
import qualified Language.EBNF.FormatHTML as H

import Control.Applicative
import Control.Monad
import Data.List

testG :: FilePath -> IO ()
testG fp = do
  inp <- readFile fp
  length inp `seq` return ()
  print $ length (lines inp)
  case parseGrammar inp of
    Left errs -> putStrLn (fmtDiagsWith inp errs)
    Right g -> do
      putStrLn $ fmtG g
      putStrLn $ "gVarsRefed: " ++ show (gVarsRefed g)
      putStrLn $ "gTermsRefed: " ++ show (gTermsRefed g)
      putStrLn "****************"
      putStrLn $ T.fmtGrammar g
      putStrLn "****************"

      putStrLn "CHECKING GRAMMAR:"
      checkGrammarIO g
      putStrLn "****************"
      -- putStrLn $ fmtGrammarHTML g
      writeFile "out.html" $ H.fmtGrammarHTML g

gio = parseGrammar' <$> readFile "TestGrammar.bnf"

checkGrammarIO :: Grammar -> IO ()
checkGrammarIO g = mapM_ checkVar (gVars g)
  where v_roots = map vName (gVars g)

        checkVar v = do
          let v_wheres  = map vName (vWhere v)
              v_refd    = eVarsRefed (vExpr v)
              v_unbound = v_refd \\ (v_roots ++ v_wheres)
              warn msg  = putStrLn $ vName v ++ ": " ++ msg
          forM_ v_unbound $ \vu ->
            warn (vu ++  " is unbound")
          forM_ (v_wheres `intersect` v_roots) $ \vsh ->
            warn (vsh ++  " shadows root variable")

