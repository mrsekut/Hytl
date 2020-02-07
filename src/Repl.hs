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
    input <- read_
    env   <- emptyEnv
    print_ $ show (eval ((parse . lexer) input) env)


-- evalRepl = repl showEval


repl :: (String -> String) -> IO ()
repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl eval


read_ :: IO String
read_ = do
    putStr "hytl> "
    hFlush stdout
    getLine


showAST :: String -> String
showAST = show . parse . lexer

showEval :: String -> String
-- showEval s = show $ emptyEnv >>= (eval ((parse . lexer) s))
showEval = showAST


print_ :: String -> IO ()
print_ = putStrLn
