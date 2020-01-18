module Main where

import           Lexer.Lexer
import           Parser.Parser
import           Parser.AST
import           Data.IORef



type Env = IORef [(String, IORef Exp)]

emptyEnv :: IO Env
emptyEnv = newIORef []


eval :: Exp -> Env -> Int
eval (Int n    ) _   = n
eval (Plus  a b) env = (eval a env) + (eval b env)
eval (Minus a b) env = (eval a env) - (eval b env)
eval (Times a b) env = (eval a env) * (eval b env)
eval (Div   a b) env = (eval a env) `quot` (eval b env)


main :: IO ()
main = do
    let f   = "x = 2"
    let ast = parse $ lexer f
    print $ "ast: " ++ show ast