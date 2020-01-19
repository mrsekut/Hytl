module Eval where

import           Lexer.Lexer
import           Parser.Parser
import           Parser.AST
import           Data.IORef



type Env = IORef [(String, Exp)]

-- emptyEnv :: Env
-- emptyEnv = newIORef []

-- envBind :: String -> Exp -> Env -> Env
-- envBind s v env = undefined

-- envLookup :: String -> Env -> Exp
-- envLookup x env = undefined


eval :: Exp -> Env -> Int
eval (Int n    ) _   = n
eval (Plus  a b) env = (eval a env) + (eval b env)
eval (Minus a b) env = (eval a env) - (eval b env)
eval (Times a b) env = (eval a env) * (eval b env)
eval (Div   a b) env = (eval a env) `quot` (eval b env)
-- eval (Assign n x) env = eval x (envBind n x env)
-- eval (Var x     ) env = eval (envLookup x env) env