module Parser.AST
    ( Exp(..)
    )
where

data Exp
    = Int Int
    | Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Lambda String String Exp
    | Call String Int
    | Var String
    deriving (Eq, Show)
