module Parser.AST
    ( Exp(..)
    , Stmt(..)
    , Program(..)
    , Op(..)
    , Pattern(..)
    , EvaledExp(..)
    )
where

newtype Program = Program [Stmt] deriving (Show, Eq)

data Stmt
    = Exp Exp
    | Assign String Exp
    deriving (Show, Eq)

data Exp
    = Nat Integer
    | Bool Bool

    | BinOp Op Exp Exp

    | List [Exp]

    | If Exp Exp Exp

    | Var String
    | Lambda [Pattern] Exp
    | App String Exp
    deriving (Eq, Show)

data Pattern
  = PVar String
  | PList [Pattern]
  | PCons Pattern Pattern
  | PBool Bool
  | PInt Integer
    deriving (Eq, Show)


data Op
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Gt
    | Ge
    | Lt
    | Le
    deriving (Eq, Show)



data EvaledExp
    = ENat Integer
    | EBool Bool
    | EString String
    | EList [EvaledExp]
    deriving (Eq, Show)
