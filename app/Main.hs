module Main where

import           Lexer.Lexer
import           Parser.Parser

eval :: Exp -> Int
eval (Int n    ) = n
eval (Plus  a b) = eval a + eval b
eval (Minus a b) = eval a + eval b
eval (Times a b) = eval a + eval b
eval (Div   a b) = eval a + eval b


main :: IO ()
main = do
    s <- getLine
    let ast = parse $ lexer s
    print $ "ast: " ++ show ast
    print $ "eval:" ++ show (eval ast)
