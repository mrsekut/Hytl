module Eval
    ( eval
    , emptyEnv
    , Env
    )
where

import           Parser.AST
import           Data.IORef
import           Data.Maybe



type Env = IORef [(String, IORef Exp)]

emptyEnv :: IO Env
emptyEnv = newIORef []

isBound :: String -> Env -> IO Bool
isBound var env = isJust . lookup var <$> readIORef env

setVar :: String -> Exp -> Env -> IO Exp
setVar var value env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> writeIORef v value
        Nothing -> return ()
    return value


envBind :: String -> Exp -> Env -> IO Exp
envBind var value env = do
    al <- isBound var env
    if al
        then (setVar var value env) >> return value
        else do
            v <- newIORef value
            e <- readIORef env
            writeIORef env ((var, v) : e)
            return value


getVar :: String -> Env -> IO Exp
getVar var env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> readIORef v
        Nothing -> return (Int (-1))


eval :: Exp -> Env -> Int
eval (Int n    ) _   = n
eval (Plus  a b) env = (eval a env) + (eval b env)
eval (Minus a b) env = (eval a env) - (eval b env)
eval (Times a b) env = (eval a env) * (eval b env)
eval (Div   a b) env = (eval a env) `quot` (eval b env)
-- eval (Assign v x) env = eval x (envBind v x env)
-- eval (Var x     ) env = eval (getVar x env) env
