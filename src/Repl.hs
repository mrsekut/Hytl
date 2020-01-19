module Repl
    ( astRepl
    , evalRepl
    )
where


import           Lexer.Lexer
import           Parser.Parser
import           Parser.AST
import           System.IO
import           Control.Monad                  ( unless )
import           System.Environment             ( getArgs )
import           Options.Applicative


astRepl :: IO ()
astRepl = repl eval_

evalRepl :: IO ()
evalRepl = repl eval_'


repl :: (String -> String) -> IO ()
repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl


read_ :: IO String
read_ = do
    putStr "hytl> "
    hFlush stdout
    getLine


eval_ :: String -> String
eval_ = show . parse . lexer

eval_' :: String -> String
-- eval_' s = show (eval ((parse . lexer) s) emptyEnv)
eval_' = eval_


print_ :: String -> IO ()
print_ = putStrLn
