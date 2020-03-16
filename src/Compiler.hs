{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import           Data.Text.Internal.Lazy
import           Data.Functor.Identity

import           LLVM.Pretty
import           LLVM.AST                hiding ( function
                                                , value
                                                )
import           LLVM.AST.Type                 as AST

import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Constant

import qualified Parser.AST                    as AST

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)


compile :: AST.Exp -> Text
compile expr = ppllvm $ buildModule "main" $ do
  form   <- globalStringPtr "%d\n" "putNumForm"
  printf <- externVarArgs "printf" [ptr i8] i32
  function "main" [] i32 $ \[] -> do
    entry <- block `named` "entry"
    do
      r <- toOperand expr
      call printf [(ConstantOperand form, []), (r, [])]
      ret (int32 0)


class LLVMOperand a where
  toOperand :: a -> LLVMBuilder Operand

instance LLVMOperand AST.Exp where
  toOperand (AST.Nat n    ) = return (int32 n)

  toOperand (AST.Add x1 x2) = do
    x1' <- toOperand x1
    x2' <- toOperand x2
    add x1' x2'
  toOperand (AST.Sub x1 x2) = do
    x1' <- toOperand x1
    x2' <- toOperand x2
    sub x1' x2'
  toOperand (AST.Mul x1 x2) = do
    x1' <- toOperand x1
    x2' <- toOperand x2
    mul x1' x2'
  toOperand (AST.Div x1 x2) = do
    x1' <- toOperand x1
    x2' <- toOperand x2
    sdiv x1' x2'
