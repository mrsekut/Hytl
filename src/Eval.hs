{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
    ( eval
    , emptyEnv
    , runEval
    , Env
    )
where

import           Parser.AST
import           Data.IORef
import           Data.Maybe
import           Control.Monad                  ( liftM )
import           Control.Monad.State





type Env = IORef [(String, Exp)]
newtype Eval a = Eval (StateT Env IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState Env
             )



{- Eval -}

runEval :: Exp -> Env -> IO Int
runEval exp = runEval' (eval exp)

runEval' :: Eval a -> Env -> IO a
runEval' (Eval m) env = evalStateT m env

eval :: Exp -> Eval Int

eval (Int  n     ) = return n
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


-- eval (Assign v x) = envBind v x
-- eval (Var x     ) = do
--     v <- getVar x
--     eval v
-- eval (Lambda f x) env = do
--     envBind f x env
--     eval (Int (-1)) env
-- eval (App f x) env = do
--     Lambda arg body <- getVar f env
--     x'              <- eval x env
--     localEnv        <- bindVars [(arg, Int x')] env
--     eval body localEnv



{- Utils -}


emptyEnv :: IO Env
emptyEnv = newIORef []


-- getVar :: String -> Eval Exp
-- getVar var = do
--     e <- get
--     case lookup var e of
--         -- Just (Var v) -> if v == var
--         --     then do
--         --         ne <- newIORef (tail e)
--         --         getVar var ne
--         --     else return (Var v)
--         Just v  -> return v
--         Nothing -> return (Int (length e))


-- envBind :: String -> Exp -> Eval Int
-- envBind var ast = do
--     -- modify ((:) (var, ast))
--     -- modify ((:) ("s", (Int 32)))
--     eval ast

-- bindVars :: [(String, Exp)] -> Env -> IO Env
-- bindVars bindings envRef = do
--     env <- readIORef envRef
--     newIORef $ bindings ++ env
