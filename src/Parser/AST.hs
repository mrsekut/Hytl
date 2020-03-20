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
    | Gt Exp Exp
    | Eq Exp Exp
    | Var String
    | Assign String Exp
    | Lambda String Exp
    | App String Exp
    | If Exp Exp Exp
    deriving (Eq, Show)
