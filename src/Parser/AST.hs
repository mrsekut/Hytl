module Parser.AST
    ( Exp(..)
    , Stmt(..)
    , Program(..)
    , Op(..)
    , Pattern(..)
    , EvaledExp(..)
    )
where
import           Data.Char (toLower)
import           Data.List (intersperse)

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
    deriving (Eq)


-- Utils
instance Show EvaledExp where
  show (ENat    i) = show i
  show (EBool   b) = map toLower $ show b
  show (EString s) = s
  show (EList list) =
    "[" ++ concat (intersperse "," $ map show list) ++ "]"
