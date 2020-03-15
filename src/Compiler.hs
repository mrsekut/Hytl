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
    toOperand (AST.Nat n) = return (int32 n)
    -- toOperand (AST.ExprTerm e      ) = toOperand e
    -- toOperand (AST.Expr t AST.Add e) = do
    --     t' <- toOperand t
    --     e' <- toOperand e
    --     add t' e'
    -- toOperand (AST.Expr t AST.Sub e) = do
    --     t' <- toOperand t
    --     e' <- toOperand e
    --     sub t' e'
    -- toOperand (AST.TermNumber n    ) = toOperand n
    -- toOperand (AST.Term n AST.Mul t) = do
    --     n' <- toOperand n
    --     t' <- toOperand t
    --     mul n' t'
    -- toOperand (AST.Term n AST.Div t) = do
    --     n' <- toOperand n
    --     t' <- toOperand t
    --     sdiv n' t'
    -- toOperand (AST.Number n) = return (int32 n)
