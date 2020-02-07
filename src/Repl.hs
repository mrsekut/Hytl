module Repl
    ( astRepl
    , evalRepl
    )
where


import           Lexer.Lexer
import           Parser.Parser
import           Parser.AST
import           Eval
import           System.IO
import           Control.Monad                  ( unless )
import           System.Environment             ( getArgs )
import           Options.Applicative


astRepl :: IO ()
astRepl = repl showAST

evalRepl :: IO ()
evalRepl = do
    env <- emptyEnv
    replEval showEval env


repl :: (String -> String) -> IO ()
repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl eval


replEval :: (String -> Env -> String) -> Env -> IO ()
replEval eval env = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input env) >> replEval eval env


read_ :: IO String
read_ = do
    putStr "hytl> "
    hFlush stdout
    getLine


showAST :: String -> String
showAST = show . parse . lexer

showEval :: String -> Env -> String
showEval s env = show (eval ((parse . lexer) s) env)


print_ :: String -> IO ()
print_ = putStrLn
