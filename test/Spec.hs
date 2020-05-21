
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
main = hspec $ describe "Parser" $ do
    spec "add" $ makeTest
        "10 + 45;"
        [TokenInt 10, TokenPlus, TokenInt 45, TokenSemicolon]
        (Program [Exp (Add (Nat 10) (Nat 45))])
        CInt
        55

    spec "sub" $ makeTest
        "42 - 10;"
        [TokenInt 42, TokenMinus, TokenInt 10, TokenSemicolon]
        (Program [Exp (Sub (Nat 42) (Nat 10))])
        CInt
        32

    spec "mul" $ makeTest
        "1 * 3;"
        [TokenInt 1, TokenTimes, TokenInt 3, TokenSemicolon]
        (Program [Exp (Mul (Nat 1) (Nat 3))])
        CInt
        3

    spec "multi" $ makeTest
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

    spec "relational" $ makeTest
        "1 > 2;"
        [TokenInt 1, TokenGT, TokenInt 2, TokenSemicolon]
        (Program [Exp (Gt (Nat 1) (Nat 2))])
        CBool
        0

    spec "assign" $ makeTest
        "x = 2*3 - 4/2;"
        [ TokenVar "x"
        , TokenAssign
        , TokenInt 2
        , TokenTimes
        , TokenInt 3
        , TokenMinus
        , TokenInt 4
        , TokenDiv
        , TokenInt 2
        , TokenSemicolon
        ]
        (Program [Assign "x" (Sub (Mul (Nat 2) (Nat 3)) (Div (Nat 4) (Nat 2)))])
        CInt
        4

    spec "function define" $ makeTest
        "f x = x + 1;"
        [ TokenVar "f"
        , TokenVar "x"
        , TokenAssign
        , TokenVar "x"
        , TokenPlus
        , TokenInt 1
        , TokenSemicolon
        ]
        (Program [Assign "f" (Lambda "x" (Add (Var "x") (Nat 1)))])
        CInt
        -1


    spec "if" $ makeTest
        "if 2>1 then 1 else 2;"
        [ TokenIf
        , TokenInt 2
        , TokenGT
        , TokenInt 1
        , TokenThen
        , TokenInt 1
        , TokenElse
        , TokenInt 2
        , TokenSemicolon
        ]
        (Program [Exp (If (Gt (Nat 2) (Nat 1)) (Nat 1) (Nat 2))])
        CInt
        1

    -- spec "list" $ makeTest
    --     "[1,2,3];"
    --     [TokenIf]
    --     (Program [Exp (If (Gt (Nat 2) (Nat 1)) (Nat 1) (Nat 2))])
    --     CInt
    --     1

    -- spec "list" $ makeTest
    --     "[1*2, 3+4, 5];"
    --     [TokenIf]
    --     (Program [Exp (If (Gt (Nat 2) (Nat 1)) (Nat 1) (Nat 2))])
    --     CInt
    --     1




makeTest :: String -> [Token] -> Program -> Constraint -> Integer -> Test
makeTest inp lex par con evl = Test { input     = inp
                                    , lexered   = lex
                                    , parsed    = par
                                    , contraint = con
                                    , evaled    = evl
                                    }


spec :: String -> Test -> SpecWith (Arg Expectation)
spec testName t = it testName $ do
    lexer (input t) `shouldBe` lexered t
    parse (lexered t) `shouldBe` parsed t
    -- typeInfer
    -- compile
    e <- runEval (eval $ parsed t) =<< emptyEnv
    e `shouldBe` evaled t



