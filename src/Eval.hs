{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
  ( eval
  , emptyEnv
  , Env
  , runEval
  , showEvaledExp
  )
where

import           Data.Char                      ( toLower )
import           Parser.AST                     ( Exp(..)
                                                , Stmt(..)
                                                , Program(..)
                                                , EvaledExp(..)
                                                )
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

class EvalC a where
  eval :: a -> Eval EvaledExp

instance EvalC Exp where
  eval (Nat  n) = return (ENat n)
  eval (Bool b) = return $ if b then (EBool True) else (EBool False)

-- eval (Add x1 x2) = (+) <$> eval x1 <*> eval x2
-- eval (Sub x1 x2) = (-) <$> eval x1 <*> eval x2
-- eval (Mul x1 x2) = (*) <$> eval x1 <*> eval x2
-- eval (Div x1 x2) = quot <$> eval x1 <*> eval x2

  -- eval (Eq x1 x2) = do
  --   n1 <- eval x1
  --   n2 <- eval x2
  --   return $ if n1 == n2 then (EBool True) else (EBool False)
  -- eval (Gt x1 x2) = do
  --   n1 <- eval x1
  --   n2 <- eval x2
  --   return $ if n1 > n2 then (EBool True) else (EBool False)
  -- eval (Ge x1 x2) = do
  --   n1 <- eval x1
  --   n2 <- eval x2
  --   return $ if n1 >= n2 then (EBool True) else (EBool False)
  -- eval (Lt x1 x2) = do
  --   n1 <- eval x1
  --   n2 <- eval x2
  --   return $ if n1 < n2 then (EBool True) else (EBool False)
  -- eval (Le x1 x2) = do
  --   n1 <- eval x1
  --   n2 <- eval x2
  --   return $ if n1 <= n2 then (EBool True) else (EBool False)

  -- eval (List [exp]) = do
  --   undefined

  -- eval (If b t e) = do
  --   cond <- eval b
  --   if cond == 1 then eval t else eval e

  -- eval (Var x          ) = eval =<< getVar x
  -- eval (Lambda arg body) = do
  --   envBind arg body
  --   return (EString "func")
  -- eval (App f x) = do
  --   exp <- getVar f
  --   case exp of
  --     Lambda arg body -> do
  --       x' <- eval x
  --       bindVars [(arg, Nat x')]
  --       eval body
  --     _ -> return $ EString "app"


instance EvalC Stmt where
  eval (Exp e     ) = eval e

  eval (Assign v x) = do
    envBind v x
    eval x


instance EvalC Program where
  eval (Program stmt) = eval $ head stmt


{- Utils -}

showEvaledExp :: EvaledExp -> String
showEvaledExp (ENat  i) = show i
showEvaledExp (EBool b) = map toLower $ show b
-- showEvaledExp (EList i) = show i


runEval :: Eval EvaledExp -> Env -> IO EvaledExp
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
  liftIO $ modifyIORef env ((++) bindings)
