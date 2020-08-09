{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
  ( eval
  , emptyEnv
  , Env
  , runEval
  , showEvaledExp
  , makeEnv
  )
where

import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State  (liftIO)
import           Control.Monad.Trans  (MonadIO)
import           Data.Char            (toLower)
import           Data.IORef           (IORef, modifyIORef, newIORef, readIORef)
import           Data.List            (intersperse)
import           Data.Maybe
import           Parser.AST           (EvaledExp (..), Exp (..), Op (..),
                                       Pattern (..), Program (..), Stmt (..))

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
  eval (Nat  n        ) = return (ENat n)
  eval (Bool b        ) = return (EBool b)

  eval (BinOp op x1 x2) = do
    e1 <- eval x1
    e2 <- eval x2
    return $ evalOp op e1 e2

  eval (List exp) = do
    list <- mapM eval exp
    return $ EList list

  eval (If b t e) = do
    cond <- eval b
    case cond of
      EBool bool -> if bool then eval t else eval e

  eval (Var x           ) = eval =<< getVar x
  eval (Lambda args body) = do
    envBind (args2key args) body
    return (EString "")
  eval (App f x) = do
    exp <- getVar f
    case exp of
      Lambda args body -> do
        case head args of -- FIXME: 単一引数のみに対応
          PList p -> do
            x' <- eval x
            let keys = ps p
            bindVars $ zi keys (evaled2exp x')
            eval body
          _ -> do
            x' <- eval x
            bindVars [(args2key args, evaled2exp x')]
            eval body
      _ -> return $ EString "app"


instance EvalC Stmt where
  eval (Exp e     ) = eval e

  eval (Assign v x) = do
    envBind v x
    eval x


instance EvalC Program where
  eval (Program stmt) = eval $ head stmt


{- Utils -}


-- FIXME: place, name, clean
ps :: [Pattern] -> [String]
ps ps = map (\(PVar x) -> x) ps

-- FIXME: place, name, clean
zi :: [String] -> Exp -> [(String, Exp)]
zi (k:ks) (List (e:es)) = (k, e) : zi ks (List es)

evalOp :: Op -> EvaledExp -> EvaledExp -> EvaledExp
evalOp Add (ENat e1) (ENat e2) = ENat (e1 + e2)
evalOp Sub (ENat e1) (ENat e2) = ENat (e1 - e2)
evalOp Mul (ENat e1) (ENat e2) = ENat (e1 * e2)
evalOp Div (ENat e1) (ENat e2) = ENat (e1 `quot` e2)
evalOp Eq  (ENat e1) (ENat e2) = if e1 == e2 then EBool True else EBool False
evalOp Gt  (ENat e1) (ENat e2) = if e1 > e2 then EBool True else EBool False
evalOp Ge  (ENat e1) (ENat e2) = if e1 >= e2 then EBool True else EBool False
evalOp Lt  (ENat e1) (ENat e2) = if e1 < e2 then EBool True else EBool False
evalOp Le  (ENat e1) (ENat e2) = if e1 <= e2 then EBool True else EBool False


evaled2exp :: EvaledExp -> Exp
evaled2exp (ENat i)    = Nat i
evaled2exp (EList exp) = List (map evaled2exp exp)


-- generate key

args2key :: [Pattern] -> String
args2key [p] = arg2key p

arg2key :: Pattern -> String
arg2key (PVar  str) = str
arg2key (PList [])  = "empty"
arg2key (PList xs)  = "xs"



showEvaledExp :: EvaledExp -> String
showEvaledExp (ENat    i) = show i
showEvaledExp (EBool   b) = map toLower $ show b
showEvaledExp (EString s) = s
showEvaledExp (EList list) =
  "[" ++ concat (intersperse "," $ map showEvaledExp list) ++ "]"


runEval :: Eval EvaledExp -> Env -> IO EvaledExp
runEval (Eval m) = runReaderT m


emptyEnv :: IO Env
emptyEnv = newIORef []

makeEnv :: [(String, Exp)] -> IO Env
makeEnv = newIORef

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
  liftIO $ modifyIORef env (bindings ++)
