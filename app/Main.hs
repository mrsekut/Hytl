module Main where

import           Lexer.Lexer
import           Parser.Parser


type Env = String -> Exp
emptyEnv = error "Not found"

envLookup :: String -> Env -> Exp
envLookup s env = env s

envBind :: String -> Exp -> Env -> Env
envBind s v env = \s' -> if s == s' then v else env s


eval :: Exp -> Env -> Int
eval (Int n       ) _   = n
eval (Plus  a b   ) env = (eval a env) + (eval b env)
eval (Minus a b   ) env = (eval a env) - (eval b env)
eval (Times a b   ) env = (eval a env) * (eval b env)
eval (Div   a b   ) env = (eval a env) `quot` (eval b env)
-- eval (Lambda n p b) env = eval b (envBind n b env)
-- eval (Call f x    ) env = eval (envLookup f env) env


main :: IO ()
main = do
    -- s <- getLine
    let f   = "fn double x = x * 2"
    let ast = parse $ lexer f
    print $ "ast: " ++ show ast
    -- print $ "eval:" ++ show (eval ast emptyEnv)

    let c    = "double 2"
    let ast2 = parse $ lexer c
    print $ "ast: " ++ show ast2
    -- print $ "eval:" ++ show (eval ast2 emptyEnv)
