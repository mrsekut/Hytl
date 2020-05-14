
import           Test.Hspec                     ( shouldBe
                                                , hspec
                                                , describe
                                                , it
                                                , SpecWith
                                                , Arg
                                                , Expectation
                                                )
import           Lexer.Lexer                    ( lexer
                                                , Token(..)
                                                )
import           Parser.Parser                  ( parse )
import           Parser.AST                     ( Exp(..)
                                                , Stmt(..)
                                                , Program(..)
                                                )
import           Compiler                       ( CodeGen )
import           Type.Type                      ( Constraint(..) )
import           Type.TypeInfer                 ( doInfers )
import           Eval                           ( eval
                                                , runEval
                                                , emptyEnv
                                                )
import           LLVM.AST                       ( Operand )

data Test = Test
     { input :: String
     , lexered :: [Token]
     , parsed ::Program
     , contraint :: Constraint
     , compiled :: Operand
     , evaled :: Integer
     }


main :: IO ()
main = do
    hspec $ describe "Parser" $ do
        spec "test!" $ makeTest
            "10 + 45;"
            [TokenInt 10, TokenPlus, TokenInt 45, TokenSemicolon]
            (Program [Exp (Add (Nat 10) (Nat 45))])
            CInt
            55

        spec "test!" $ makeTest
            "42 - 10;"
            [TokenInt 42, TokenMinus, TokenInt 10, TokenSemicolon]
            (Program [Exp (Sub (Nat 42) (Nat 10))])
            CInt
            32

        spec "test!" $ makeTest
            "1 * 3;"
            [TokenInt 1, TokenTimes, TokenInt 3, TokenSemicolon]
            (Program [Exp (Mul (Nat 1) (Nat 3))])
            CInt
            3

        spec "test!" $ makeTest
            "10 + 1 * 3;"
            [ TokenInt 10
            , TokenPlus
            , TokenInt 1
            , TokenTimes
            , TokenInt 3
            , TokenSemicolon
            ]
            (Program [Exp (Add (Nat 10) (Mul (Nat 1) (Nat 3)))])
            CInt
            13


makeTest :: String -> [Token] -> Program -> Constraint -> Integer -> Test
makeTest inp lex par con evl =
    Test { input = inp, lexered = lex, parsed = par, contraint = con, evaled = evl }


spec :: String -> Test -> SpecWith (Arg Expectation)
spec testName t = it testName $ do
    lexer (input t) `shouldBe` lexered t
    parse (lexered t) `shouldBe` parsed t
    -- typeInfer
    -- compile
    e <- runEval (eval $ parsed t) =<< emptyEnv
    e `shouldBe` evaled t


-- main :: IO ()
-- main = hspec $ describe "Parser" $ do
--     it "Add, Sub, Mul, Div" $ do
--         showAST "1 + 1" `shouldBe` Plus (Int 1) (Int 1)
--         showAST "2 * 3" `shouldBe` Times (Int 2) (Int 3)
--         showAST "1 + 2 - 3" `shouldBe` Minus (Plus (Int 1) (Int 2)) (Int 3)
--         showAST "1 * 2 / 3" `shouldBe` Div (Times (Int 1) (Int 2)) (Int 3)

--     it "relational" $ do
--         showAST "1 > 2" `shouldBe` Gt (Int 1) (Int 2)
--         showAST "1*2 > 2+3"
--             `shouldBe` Gt (Times (Int 1) (Int 2)) (Plus (Int 2) (Int 3))

--     it "Assign" $ do
--         showAST "x = 2*3-2/3" `shouldBe` Assign
--             "x"
--             (Minus (Times (Int 2) (Int 3)) (Div (Int 2) (Int 3)))
--         showAST "x" `shouldBe` Var "x"

--     it "if" $ do
--         showAST "if 2>1 then 1 else 2"
--             `shouldBe` If (Gt (Int 2) (Int 1)) (Int 4) (Int 5)


