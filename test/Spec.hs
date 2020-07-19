
import           Test.Hspec                     ( shouldBe
                                                , hspec
                                                , describe
                                                , it
                                                )
import qualified Lexer.Lexer                   as L
import qualified Parser.Parser                 as P
import           Parser.AST                     ( Exp(..)
                                                , Stmt(..)
                                                , Program(..)
                                                , Op(..)
                                                , Pattern(..)
                                                )

main :: IO ()
main = lexerAndParser



lexerAndParser :: IO ()
lexerAndParser = hspec $ do
    describe "lexer and parser test" $ do
        it "add" $ do
            (P.parse . L.lexer) "10 + 42"
                `shouldBe` (Program [Exp (BinOp Add (Nat 10) (Nat 42))])
        it "sub" $ do
            (P.parse . L.lexer) "42 - 10"
                `shouldBe` (Program [Exp (BinOp Sub (Nat 42) (Nat 10))])
        it "mul" $ do
            (P.parse . L.lexer) "1 * 3"
                `shouldBe` (Program [Exp (BinOp Mul (Nat 1) (Nat 3))])
        it "relational" $ do
            (P.parse . L.lexer) "1 > 2"
                `shouldBe` (Program [Exp (BinOp Gt (Nat 1) (Nat 2))])
        it "assign" $ do
            (P.parse . L.lexer) "x = 2*3 - 4/2"
                `shouldBe` (Program
                               [ Assign
                                     "x"
                                     (BinOp Sub
                                            (BinOp Mul (Nat 2) (Nat 3))
                                            (BinOp Div (Nat 4) (Nat 2))
                                     )
                               ]
                           )
        it "if" $ do
            (P.parse . L.lexer) "if 2>1 then 1 else 2"
                `shouldBe` (Program
                               [ Exp
                                     (If (BinOp Gt (Nat 2) (Nat 1))
                                         (Nat 1)
                                         (Nat 2)
                                     )
                               ]
                           )
        it "list" $ do
            (P.parse . L.lexer) "[1,2,3]"
                `shouldBe` (Program [Exp (List [Nat 1, Nat 2, Nat 3])])
        it "list" $ do
            (P.parse . L.lexer) "[1*2, 3+4, 5]"
                `shouldBe` (Program
                               [ Exp
                                     (List
                                         [ BinOp Mul (Nat 1) (Nat 2)
                                         , BinOp Add (Nat 3) (Nat 4)
                                         , Nat 5
                                         ]
                                     )
                               ]
                           )


    describe "defined functions" $ do
        it "normal" $ do
            (P.parse . L.lexer) "f x = x + 1"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda [PVar "x"]
                                             (BinOp Add (Var "x") (Nat 1))
                                     )
                               ]
                           )
        it "const int arg" $ do
            (P.parse . L.lexer) "f 10 = 5"
                `shouldBe` (Program [Assign "f" (Lambda [PInt 10] (Nat 5))])
        it "const bool arg" $ do
            (P.parse . L.lexer) "f true = false"
                `shouldBe` (Program
                               [Assign "f" (Lambda [PBool True] (Bool False))]
                           )
        it "empty arg" $ do
            (P.parse . L.lexer) "f [] = []"
                `shouldBe` (Program [Assign "f" (Lambda [PList []] (List []))])
        it "1 element arg" $ do
            (P.parse . L.lexer) "f [x] = x"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda [PList [PVar "x"]] (Var "x"))
                               ]
                           )
        it "1 element arg with paren" $ do
            (P.parse . L.lexer) "f ([x]) = x"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda [PList [PVar "x"]] (Var "x"))
                               ]
                           )
        it "2 elements arg" $ do
            (P.parse . L.lexer) "f [x,y] = x"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda [PList [PVar "x", PVar "y"]]
                                             (Var "x")
                                     )
                               ]
                           )
        it "1 int element arg" $ do
            (P.parse . L.lexer) "f [1] = 1"
                `shouldBe` (Program
                               [Assign "f" (Lambda [PList [PInt 1]] (Nat 1))]
                           )
        it "1 int element arg with paren" $ do
            (P.parse . L.lexer) "f ([1]) = 1"
                `shouldBe` (Program
                               [Assign "f" (Lambda [PList [PInt 1]] (Nat 1))]
                           )
        it "1 bool element arg" $ do
            (P.parse . L.lexer) "f [true] = true"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda [PList [PBool True]] (Bool True))
                               ]
                           )
        it "cons arg" $ do
            (P.parse . L.lexer) "f (x:xs) = xs"
                `shouldBe` (Program
                               [ Assign
                                     "f"
                                     (Lambda
                                         [PList [(PVar "x"), (PVar "xs")]]
                                         (Var "xs")
                                     )
                               ]
                           )
        -- it "cons arg" $ do
        --     (P.parse . L.lexer) "f (1:xs) = xs"
        --         `shouldBe` (Program
        --                        [ Assign
        --                              "f"
        --                              (Lambda [PCons (PInt 1) (PVar "xs")]
        --                                      (Var "xs")
        --                              )
        --                        ]
        --                    )
        -- it "cons arg" $ do
        --     (P.parse . L.lexer) "f (1:[2,3]) = 1"
        --         `shouldBe` (Program
        --                        [ Assign
        --                              "f"
        --                              (Lambda [PCons (PInt 1) (PList [PInt 2, PInt 3])]
        --                                      (Var "xs")
        --                              )
        --                        ]
        --                    )
        -- it "cons arg" $ do
        --     (P.parse . L.lexer) "f (x:y:ys) = ys"
        --         `shouldBe` (Program
        --                        [ Assign
        --                              "f"
        --                              (Lambda [PCons (PVar "x") (Cons (PVar "y") (PVar "ys"))]
        --                                      (Var "xs")
        --                              )
        --                        ]
        --                    )

    -- describe "call functions" $ do
    --     it "normal" $ do
    --         (P.parse . L.lexer) "f 1"
    --             `shouldBe` (Program [Exp (App "f" (Nat 1))])
    --     it "empty" $ do
    --         (P.parse . L.lexer) "f []"
    --             `shouldBe` (Program [Exp (App "f" (List []))])


-- typeInfer ::
-- compiler ::
-- eval ::
