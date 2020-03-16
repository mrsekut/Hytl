module Repl
  ( astRepl
  , evalRepl
  )
where


import           Lexer.Lexer
import           Parser.Parser
import           Eval
import           System.IO
import           Control.Monad                  ( unless )


{- AST mode -}

astRepl :: IO ()
astRepl = repl showAST
 where
  showAST :: String -> String
  showAST = show . parse . lexer

  repl :: (String -> String) -> IO ()
  repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl eval




{- Eval mode -}

evalRepl :: IO ()
evalRepl = replIO showEval =<< emptyEnv
 where
  showEval :: String -> Env -> IO Integer
  showEval s = runEval $ eval ((parse . lexer) s)

  replIO :: (String -> Env -> IO Integer) -> Env -> IO ()
  replIO eval env = do
    input <- read_
    unless (input == ":quit") $ do
      value <- eval input env
      print_ $ show value
      replIO eval env


{- Utils -}

read_ :: IO String
read_ = do
  putStr "hytl> "
  hFlush stdout
  getLine

print_ :: String -> IO ()
print_ = putStrLn
