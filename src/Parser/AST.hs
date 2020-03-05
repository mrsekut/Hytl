module Parser.AST
    ( Exp(..)
    )
where

data Exp
    = Int Int
    | Bool Bool
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Var String
    | Assign String Exp
    | Lambda String Exp
    | Call String Exp
    | If Exp Exp Exp
    deriving (Eq, Show)
