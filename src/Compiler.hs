{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler where

import           Data.Text.Internal.Lazy
import           Data.Functor.Identity
import qualified Data.Map                      as M
import           Control.Monad.State     hiding ( void )
import           Data.Maybe

import           LLVM.Pretty
import           LLVM.AST                hiding ( function
                                                , value
                                                )
import           LLVM.AST.Type                 as AST
import qualified LLVM.AST.IntegerPredicate     as IP


import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Constant

import qualified Parser.AST                    as AST


newtype GenState = GenState { table :: M.Map String AST.Exp}
type CodeGen a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (State GenState)



compile :: AST.Program -> Text
compile expr = ppllvm $ evalState
  (buildModuleT "main" $ function "main" [] i32 $ \_ -> mdo
    form     <- globalStringPtr "%d\n" "putNumForm"
    printf   <- externVarArgs "printf" [ptr i8] i32
    operands <- toOperands expr
    mapM (callPrintf form printf) operands
    ret (int32 0)
  )
  emptyCodegen
 where
  callPrintf form printf r = call printf [(ConstantOperand form, []), (r, [])]


emptyCodegen :: GenState
emptyCodegen = GenState $ M.fromList []


class LLVMOperand a where
  toOperand :: a -> CodeGen Operand
  toOperands :: a -> CodeGen [Operand]


instance LLVMOperand AST.Exp where
  toOperand (AST.Nat n    ) = return (int32 n)

  toOperand (AST.Add x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    add x1' x2'
  toOperand (AST.Sub x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    sub x1' x2'
  toOperand (AST.Mul x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    mul x1' x2'
  toOperand (AST.Div x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    sdiv x1' x2'

  toOperand (AST.Eq x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    icmp IP.EQ x1' x2' -- 1==true, 0==false
  toOperand (AST.Gt x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    icmp IP.UGT x1' x2' -- 1==true, 0==false
  toOperand (AST.Ge x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    icmp IP.UGE x1' x2' -- 1==true, 0==false
  toOperand (AST.Lt x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    icmp IP.ULT x1' x2' -- 1==true, 0==false
  toOperand (AST.Le x1 x2) = mdo
    x1' <- toOperand x1
    x2' <- toOperand x2
    icmp IP.ULE x1' x2' -- 1==true, 0==false


  toOperand (AST.Var x          ) = toOperand =<< getVar x
  toOperand (AST.Lambda arg body) = mdo
    envBind arg body
    toOperand (AST.Nat (-1))
  toOperand (AST.App f x) = do
    exp <- getVar f
    case exp of
      AST.Lambda arg body -> do
        bindVars [(arg, x)]
        toOperand body
      _ -> toOperand (AST.Nat (-1))


instance LLVMOperand AST.Stmt where
  toOperand (AST.Exp e       ) = toOperand e

  toOperand (AST.Assign s exp) = mdo
    envBind s exp
    toOperand exp


instance LLVMOperand AST.Program where
  toOperands (AST.Program stmt) = mapM toOperand stmt


{- Utils -}

getVar :: String -> CodeGen AST.Exp
getVar var = do
  st <- get
  case M.lookup var $ table st of
    Just v  -> return v
    Nothing -> return (AST.Nat $ (toInteger . length) (table st))


envBind :: String -> AST.Exp -> CodeGen ()
envBind var ast = do
  GenState st <- get
  put $ GenState $ M.insert var ast st


bindVars :: [(String, AST.Exp)] -> CodeGen ()
bindVars bindings = do
  GenState st <- get
  put $ GenState $ M.union (M.fromList bindings) st
