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

-- eval
eval :: Exp -> Env -> IO Int
eval (Int n     ) _   = return n
eval (Plus x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ n1 + n2
eval (Minus x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ n1 - n2
eval (Times x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ n1 * n2
eval (Div x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ n1 `quot` n2
eval (Var x) env = do
    v <- getVar x env
    eval v env
eval (Assign v x) env = do
    envBind v x env
    eval x env


emptyEnv :: IO Env
emptyEnv = newIORef []

getVar :: String -> Env -> IO Exp
getVar var env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> readIORef v
        Nothing -> return (Var "error")

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


isBound :: String -> Env -> IO Bool
isBound var env = isJust . lookup var <$> readIORef env

setVar :: String -> Exp -> Env -> IO Exp
setVar var value env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> writeIORef v value
        Nothing -> return ()
    return value

