module Main where

import           Lexer.Lexer
import           Parser.Parser
import           Parser.AST
import           Data.IORef
import qualified Data.Map                      as M



type Env = M.Map String Exp

emptyEnv :: Env
emptyEnv = M.empty

envBind :: String -> Exp -> Env -> Env
envBind s v env = M.insert s v env

envLookup :: String -> Env -> Exp
envLookup x env = case M.lookup x env of
    Just exp -> exp
    Nothing  -> (Int (-1)) -- error



eval :: Exp -> Env -> Int
eval (Int n     ) _   = n
eval (Plus   a b) env = (eval a env) + (eval b env)
eval (Minus  a b) env = (eval a env) - (eval b env)
eval (Times  a b) env = (eval a env) * (eval b env)
eval (Div    a b) env = (eval a env) `quot` (eval b env)
eval (Assign n x) env = eval x (envBind n x env)
eval (Var x     ) env = eval (envLookup x env) env


main :: IO ()
main = do
    let f   = "x = 2"
    let ast = parse $ lexer f
    print $ "ast: " ++ show ast
    print $ "eval: " ++ show (eval ast emptyEnv)

    let f   = "x + 2"
    let ast = parse $ lexer f
    print $ "ast: " ++ show ast
    print $ "eval: " ++ show (eval ast emptyEnv)
