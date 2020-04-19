module Type.Type
    ( Constraint(..)
    )
where

data Constraint
  = CInt
  | CBool
  | CVar Int
  | CLambda Constraint Constraint
  deriving (Show, Eq)
