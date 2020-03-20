module Parser.AST
    ( Exp(..)
    )
where

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

    | Var String
    | Assign String Exp
    | Lambda String Exp
    | App String Exp
    | If Exp Exp Exp
    deriving (Eq, Show)
