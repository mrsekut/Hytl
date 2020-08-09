
import qualified Eval          as E
import qualified Lexer.Lexer   as L
import           Parser.AST    (Exp (..), Op (..), Pattern (..), Program (..),
                                Stmt (..))
import qualified Parser.Parser as P
import           Test.Hspec    (describe, hspec, it, shouldBe)

main :: IO ()
main = do
    lexerAndParser
    testEval


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
        it "exps list" $ do
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
        it "cons" $ do
            (P.parse . L.lexer) "1:2:3:[]"
                `shouldBe` (Program [Exp (List [Nat 1, Nat 2, Nat 3])])
        it "empty" $ do
            (P.parse . L.lexer) "[]"
                `shouldBe` (Program [Exp (List [])])
        it "multi list" $ do
            (P.parse . L.lexer) "[1,2,[3]]"
                `shouldBe` (Program [Exp (List [Nat 1, Nat 2, List [Nat 3]])])
        it "multi cons" $ do
            (P.parse . L.lexer) "[3]:[]"
                `shouldBe` (Program [Exp (List [List [Nat 3]])])


    describe "defined functions" $ do
        it "normal" $ do
            (P.parse . L.lexer) "f x = x + 1"
                `shouldBe` (Program [ Assign "f" (Lambda [PVar "x"] (BinOp Add (Var "x") (Nat 1))) ])
        -- it "const int arg" $ do
        --     (P.parse . L.lexer) "f 10 = 5"
        --         `shouldBe` (Program [Assign "f" (Lambda [PInt 10] (Nat 5))])
        -- it "const bool arg" $ do
        --     (P.parse . L.lexer) "f true = false"
        --         `shouldBe` (Program [Assign "f" (Lambda [PBool True] (Bool False))])
        it "empty arg" $ do
            (P.parse . L.lexer) "f [] = []"
                `shouldBe` (Program [Assign "f" (Lambda [PList []] (List []))])
        -- it "1 element arg" $ do
        --     (P.parse . L.lexer) "f [x] = x"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PList [PVar "x"]] (Var "x")) ])
        -- it "1 element arg with paren" $ do
        --     (P.parse . L.lexer) "f ([x]) = x"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PList [PVar "x"]] (Var "x")) ])
        -- it "2 elements arg" $ do
        --     (P.parse . L.lexer) "f [x,y] = x"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PList [PVar "x", PVar "y"]] (Var "x")) ])
        -- it "1 int element arg" $ do
        --     (P.parse . L.lexer) "f [1] = 1"
        --         `shouldBe` (Program [Assign "f" (Lambda [PList [PInt 1]] (Nat 1))])
        -- it "1 int element arg with paren" $ do
        --     (P.parse . L.lexer) "f ([1]) = 1"
        --         `shouldBe` (Program [Assign "f" (Lambda [PList [PInt 1]] (Nat 1))])
        -- it "1 bool element arg" $ do
        --     (P.parse . L.lexer) "f [true] = true"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PList [PBool True]] (Bool True)) ])
        it "cons arg" $ do
            (P.parse . L.lexer) "f (x:xs) = xs"
                `shouldBe` (Program [ Assign "f" (Lambda [PList [(PVar "x"), (PVar "xs")]] (Var "xs")) ])
        -- it "cons arg" $ do
        --     (P.parse . L.lexer) "f (1:xs) = xs"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PCons (PInt 1) (PVar "xs")] (Var "xs")) ])
        -- it "cons arg" $ do
        --     (P.parse . L.lexer) "f (1:[2,3]) = 1"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PCons (PInt 1) (PList [PInt 2, PInt 3])] (Var "xs")) ])
        -- it "cons arg" $ do

    -- describe "call functions" $ do
    --     it "normal" $ do
    --         (P.parse . L.lexer) "f 1"
    --             `shouldBe` (Program [Exp (App "f" (Nat 1))])
    --     it "empty" $ do
    --         (P.parse . L.lexer) "f []"
    --             `shouldBe` (Program [Exp (App "f" (List []))])


testEval :: IO ()
testEval = hspec $ do
    describe "eval test" $ do
        it "add" $ do
            "10 + 42" `evalShouldBe` "52"
        it "sub" $ do
            "42 - 10" `evalShouldBe` "32"
        it "mul" $ do
            "1 * 3" `evalShouldBe` "3"
        it "relational" $ do
            "1 > 2" `evalShouldBe` "false"
        it "assign" $ do
            "x = 2*3 - 4/2" `evalShouldBe` "4"
        it "if" $ do
            "if 2>1 then 1 else 2" `evalShouldBe` "1"

        it "list" $ do
            "[1,2,3]" `evalShouldBe` "[1,2,3]"
        it "list" $ do
            "[1*2, 3+4, 5]" `evalShouldBe` "[2,7,5]"

    -- describe "defined functions" $ do
    --     it "normal" $ do
    --         "f x = x + 1" `evalShouldBe` "FIXME:"
    --     it "const int arg" $ do
    --         "f 10 = 5" `evalShouldBe` "FIXME:"
    --     it "const bool arg" $ do
    --         "f true = false" `evalShouldBe` "FIXME:"
    --     it "empty arg" $ do
    --         "f [] = []" `evalShouldBe` "FIXME:"
    --     it "1 element arg" $ do
    --         "f [x] = x" `evalShouldBe` "FIXME:"
    --     it "1 element arg with paren" $ do
    --         "f ([x]) = x" `evalShouldBe` "FIXME:"
    --     it "2 elements arg" $ do
    --         "f [x,y] = x" `evalShouldBe` "FIXME:"
    --     it "1 int element arg" $ do
    --         "f [1] = 1" `evalShouldBe` "FIXME:"
    --     it "1 int element arg with paren" $ do
    --         "f ([1]) = 1" `evalShouldBe` "FIXME:"
    --     it "1 bool element arg" $ do
    --         "f [true] = true" `evalShouldBe` "FIXME:"
    --     it "cons arg" $ do
    --         "f (x:xs) = xs" `evalShouldBe` "FIXME:"

-- typeInfer ::
-- compiler ::
-- eval ::


-- | Utils

evalShouldBe :: String -> String -> IO ()
evalShouldBe input result = do
    env   <- E.emptyEnv
    value <- E.runEval ((E.eval . P.parse . L.lexer) input) env
    (E.showEvaledExp value) `shouldBe` result
