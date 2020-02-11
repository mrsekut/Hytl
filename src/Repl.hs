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


-- ast mode
astRepl :: IO ()
astRepl = do
    env <- emptyEnv
    repl showAST env

showAST :: String -> Env -> String
showAST s _ = (show . parse . lexer) s


-- eval mode
evalRepl :: IO ()
evalRepl = do
    env <- emptyEnv
    repl showEval env

showEval :: String -> Env -> String
showEval s env = show (eval ((parse . lexer) s) env)


-- core
read_ :: IO String
read_ = do
    putStr "hytl> "
    hFlush stdout
    getLine

print_ :: String -> IO ()
print_ = putStrLn

repl :: (String -> Env -> String) -> Env -> IO ()
repl eval env = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input env) >> repl eval env

