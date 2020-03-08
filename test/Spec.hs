{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import           Test.Hspec
import           Lexer.Lexer
import           Parser.AST
import           Parser.Parser
import           Eval


showAST :: String -> Exp
showAST = parse . lexer


main :: IO ()
main = hspec $ describe "Parser" $ do
    it "Add, Sub, Mul, Div" $ do
        showAST "1 + 1" `shouldBe` Plus (Int 1) (Int 1)
        showAST "2 * 3" `shouldBe` Times (Int 2) (Int 3)
        showAST "1 + 2 - 3" `shouldBe` Minus (Plus (Int 1) (Int 2)) (Int 3)
        showAST "1 * 2 / 3" `shouldBe` Div (Times (Int 1) (Int 2)) (Int 3)

    it "relational" $ do
        showAST "1 > 2" `shouldBe` Gt (Int 1) (Int 2)
        showAST "1*2 > 2+3"
            `shouldBe` Gt (Times (Int 1) (Int 2)) (Plus (Int 2) (Int 3))

    it "Assign" $ do
        showAST "x = 2*3-2/3" `shouldBe` Assign
            "x"
            (Minus (Times (Int 2) (Int 3)) (Div (Int 2) (Int 3)))
        showAST "x" `shouldBe` Var "x"

    it "if" $ do
        showAST "if 2>1 then 1 else 2"
            `shouldBe` If (Gt (Int 2) (Int 1)) (Int 4) (Int 5)
