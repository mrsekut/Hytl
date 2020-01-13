module Main where

import           Lexer.Lexer
import           Parser.Parser

main :: IO ()
main = do
    s <- getLine
    let ast = parse $ lexer s
    print ast
