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
import           System.Environment             ( getArgs )


-- ast mode
astRepl :: IO ()
astRepl = repl showAST

showAST :: String -> String
showAST = show . parse . lexer

repl :: (String -> String) -> IO ()
repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl eval



-- eval mode
evalRepl :: IO ()
evalRepl = do
    env <- emptyEnv
    replEval showEval env

showEval :: String -> Env -> String
showEval s env = show (eval ((parse . lexer) s) env)

replEval :: (String -> Env -> String) -> Env -> IO ()
replEval eval env = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input env) >> replEval eval env


-- core
read_ :: IO String
read_ = do
    putStr "hytl> "
    hFlush stdout
    getLine

print_ :: String -> IO ()
print_ = putStrLn
