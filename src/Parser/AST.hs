module Parser.AST
    ( Exp(..)
    )
where

data Exp
    = Nat Integer
    | Bool Bool
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Gt Exp Exp
    | Var String
    | Assign String Exp
    | Lambda String Exp
    | App String Exp
    | If Exp Exp Exp
    deriving (Eq, Show)
