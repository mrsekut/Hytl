{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Compiler
  ( CodeGen
  , compile
  )
where

import           Control.Monad.State        hiding (void)
import           Data.Functor.Identity
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text.Internal.Lazy

import           LLVM.AST                   hiding (function, value)
import qualified LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Type
import           LLVM.Pretty


import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import qualified Parser.AST                 as AST


newtype GenState = GenState { table :: M.Map String AST.Exp}
type CodeGen a = IRBuilderT GenDec a
type GenDec = ModuleBuilderT (State GenState)



compile :: AST.Program -> Text
compile expr = ppllvm $ evalState
  (buildModuleT "program" $ do
    -- genFunction expr
    genMain expr
  )
  emptyCodegen


genMain :: LLVMOperand a => a -> GenDec Operand
genMain expr = function "main" [] i32 $ \_ -> block `named` "entry" >> mdo
  form   <- globalStringPtr "%d\n" "putNumForm"
  printf <- externVarArgs "printf" [ptr i8] i32
  let callPrintf r = call printf [(ConstantOperand form, []), (r, [])]

  operands <- toOperands expr
  mapM callPrintf operands

  ret $ int32 0


genFunction :: LLVMOperand a => a -> GenDec Operand
genFunction expr = function "func" [] i32 $ \_ -> mdo
  operands <- toOperands expr
  ret $ last operands


emptyCodegen :: GenState
emptyCodegen = GenState $ M.fromList []


class LLVMOperand a where
  toOperand :: a -> CodeGen Operand
  toOperands :: a -> CodeGen [Operand]


instance LLVMOperand AST.Exp where
  toOperand (AST.Nat  n  ) = return (int32 n)
  toOperand (AST.Bool b  ) = return $ if b then bit 1 else bit 0 -- 1==true, 0==false

-- toOperand (AST.Add x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   add x1' x2'
-- toOperand (AST.Sub x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   sub x1' x2'
-- toOperand (AST.Mul x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   mul x1' x2'
-- toOperand (AST.Div x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   sdiv x1' x2'

-- toOperand (AST.Eq x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   icmp IP.EQ x1' x2' -- 1==true, 0==false
-- toOperand (AST.Gt x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   icmp IP.UGT x1' x2' -- 1==true, 0==false
-- toOperand (AST.Ge x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   icmp IP.UGE x1' x2' -- 1==true, 0==false
-- toOperand (AST.Lt x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   icmp IP.ULT x1' x2' -- 1==true, 0==false
-- toOperand (AST.Le x1 x2) = mdo
--   x1' <- toOperand x1
--   x2' <- toOperand x2
--   icmp IP.ULE x1' x2' -- 1==true, 0==false

  toOperand (AST.If b t e) = mdo
    cond  <- toOperand b
    then' <- freshName "then"
    else' <- freshName "else"

    condBr cond then' else'

    emitBlockStart then'
    toOperand t

    emitBlockStart else'
    toOperand e

-- if cond == bit 1 then toOperand t else toOperand e

  toOperand (AST.Var x) = toOperand =<< getVar x

-- toOperand (AST.Lambda arg body) = mdo
--   envBind arg body
--   toOperand (AST.Nat (-1))

  -- toOperand (AST.App f x) = mdo
  --   exp <- getVar f
  --   case exp of
  --     AST.Lambda arg body -> mdo
  --       bindVars [(arg, x)]
  --       toOperand body
  --     _ -> toOperand (AST.Nat (-1))


instance LLVMOperand AST.Stmt where
  toOperand (AST.Exp e          ) = toOperand e

  toOperand (AST.Assign name exp) = mdo
    toOperand exp
    envBind name exp
    toOperand (AST.Nat (-1))

instance LLVMOperand AST.Program where
  toOperands (AST.Program stmt) = mapM toOperand stmt


{- Utils -}

getVar :: String -> CodeGen AST.Exp
getVar name = do
  st <- get
  case M.lookup name $ table st of
    Just v  -> return v
    Nothing -> fail $ "Var " ++ name ++ " not defeined"


envBind :: String -> AST.Exp -> CodeGen ()
envBind var ast = do
  GenState st <- get
  put $ GenState $ M.insert var ast st


bindVars :: [(String, AST.Exp)] -> CodeGen ()
bindVars bindings = do
  GenState st <- get
  put $ GenState $ M.union (M.fromList bindings) st
