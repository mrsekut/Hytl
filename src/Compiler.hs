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

import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Constant

import qualified Parser.AST                    as AST


data GenState = GenState { table :: M.Map String AST.Exp}
type CodeGen a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (State GenState)



compile :: AST.Exp -> Text
compile expr = ppllvm $ evalState
  (buildModuleT "main" $ function "main" [] i32 $ \_ -> mdo
    r <- toOperand expr
    printf r
    ret (int32 0)
  )
  emptyCodegen
 where
  printf r = do
    form   <- globalStringPtr "%d\n" "putNumForm"
    printf <- externVarArgs "printf" [ptr i8] i32
    call printf [(ConstantOperand form, []), (r, [])]


emptyCodegen :: GenState
emptyCodegen = GenState $ M.fromList []


class LLVMOperand a where
  toOperand :: a -> CodeGen Operand

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

  toOperand (AST.Assign s exp) = mdo
    let li = M.fromList [(s, exp)]
    modify (\s -> s { table = li })
    toOperand exp
  toOperand (AST.Var x) = mdo
    a <- get
    toOperand $ fromJust $ M.lookup x $ table a

-- addTable :: (MonadState GenState m, MonadTrans t) => String -> Operand -> t m ()
-- addTable name opr = lift (modify (\s -> s { table = set name opr }))
