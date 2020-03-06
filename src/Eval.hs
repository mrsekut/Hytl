module Eval
    ( eval
    , emptyEnv
    , Env
    )
where

import           Parser.AST
import           Data.IORef
import           Data.Maybe
import           Control.Monad                  ( liftM )


type Env = IORef [(String, IORef Exp)]



{- Eval -}

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
eval (Bool b) env = do
    return $ if b then 1 else 0 -- 1==true, 0==false
eval (Gt x1 x2) env = do
    n1 <- eval x1 env
    n2 <- eval x2 env
    return $ if n1 > n2 then 1 else 0 -- 1==true, 0==false
eval (If b t e) env = do
    cond <- eval b env
    thn  <- eval t env
    els  <- eval e env
    return $ if cond == 1 then thn else els
eval (Var x) env = do
    v <- getVar x env
    eval v env
eval (Assign v x) env = do
    envBind v x env
    eval x env
eval (Lambda f x) env = do
    envBind f x env
    eval (Int (-1)) env
eval (Call f x) env = do
    Lambda arg body <- getVar f env
    localEnv        <- bindVars [(arg, x)] env
    eval body localEnv



{- Utils -}


emptyEnv :: IO Env
emptyEnv = newIORef []


getVar :: String -> Env -> IO Exp
getVar var env = do
    e <- readIORef env
    case lookup var e of
        Just v  -> readIORef v
        Nothing -> return (Var "error")


envBind :: String -> Exp -> Env -> IO Exp
envBind var ast env = do
    hasVar <- isBound var env
    if hasVar
        then setVar var ast env >> return ast
        else do
            a <- newIORef ast
            e <- readIORef env
            writeIORef env ((var, a) : e)
            return ast
  where
    isBound :: String -> Env -> IO Bool
    isBound var env = isJust . lookup var <$> readIORef env

    setVar :: String -> Exp -> Env -> IO Exp
    setVar var ast env = do
        e <- readIORef env
        case lookup var e of
            Just v  -> writeIORef v ast
            Nothing -> return ()
        return ast


bindVars :: [(String, Exp)] -> Env -> IO Env
bindVars bindings envRef = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)
