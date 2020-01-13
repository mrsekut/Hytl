module Main where

import           Lexer.Lexer
import           Parser.Parser

main :: IO ()
main = do
    s <- getLine
    let ast = happyParser $ alexScanTokens s
    print ast
