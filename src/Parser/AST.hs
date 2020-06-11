module Parser.AST
    ( Exp(..)
    , Stmt(..)
    , Program(..)
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

    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp

    | Eq Exp Exp
    | Gt Exp Exp
    | Ge Exp Exp
    | Lt Exp Exp
    | Le Exp Exp

    | List [Exp]

    | If Exp Exp Exp

    | Var String
    | Lambda String Exp
    | App String Exp
    deriving (Eq, Show)


data EvaledExp
    = ENat Integer
    | EBool Bool
    | EString String
    | EList [EvaledExp]
