{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
    ( eval
    , emptyEnv
    , Env
    , Eval
    , runEval
    )
where

import           Parser.AST
import           Data.IORef
import           Data.Maybe
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , MonadReader
                                                , ask
                                                )
import           Control.Monad.Trans            ( MonadIO )
import           Control.Monad.State            ( liftIO )


type Env = IORef [(String, Exp)]
newtype Eval a = Eval (ReaderT Env IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             )



{- Eval -}

eval :: Exp -> Eval Integer
eval (Nat  n     ) = return n
eval (Bool b     ) = return $ if b then 1 else 0 -- 1==true, 0==false

eval (Plus  x1 x2) = (+) <$> eval x1 <*> eval x2
eval (Minus x1 x2) = (-) <$> eval x1 <*> eval x2
eval (Times x1 x2) = (*) <$> eval x1 <*> eval x2
eval (Div   x1 x2) = quot <$> eval x1 <*> eval x2

eval (Gt    x1 x2) = do
    n1 <- eval x1
    n2 <- eval x2
    return $ if n1 > n2 then 1 else 0 -- 1==true, 0==false

eval (If b t e) = do
    cond <- eval b
    if cond == 1 then eval t else eval e

eval (Assign v x) = do
    envBind v x
    eval x
eval (Var x) = do
    v <- getVar x
    eval v
eval (Lambda arg body) = do
    envBind arg body
    eval (Nat (-1))
eval (App f x) = do
    exp <- getVar f
    case exp of
        Lambda arg body -> do
            x' <- eval x
            bindVars [(arg, Nat x')]
            eval body
        _ -> return (-1)



{- Utils -}

runEval :: Eval a -> Env -> IO a
runEval (Eval m) = runReaderT m


emptyEnv :: IO Env
emptyEnv = newIORef []


getVar :: String -> Eval Exp
getVar var = do
    env <- ask
    e   <- liftIO $ readIORef env
    case lookup var e of
        Just v  -> return v
        Nothing -> return (Nat $ (toInteger . length) e)


envBind :: String -> Exp -> Eval ()
envBind var ast = do
    env <- ask
    liftIO $ modifyIORef env ((:) (var, ast))


bindVars :: [(String, Exp)] -> Eval ()
bindVars bindings = do
    env <- ask
    e   <- liftIO $ readIORef env
    liftIO $ modifyIORef env ((++) bindings)
