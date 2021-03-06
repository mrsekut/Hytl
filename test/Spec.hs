import qualified Eval           as E
import qualified Lexer.Lexer    as L
import           Parser.AST     (Exp (..), Op (..), Pattern (..), Program (..),
                                 Stmt (..))
import qualified Parser.Parser  as P
import           Test.Hspec     (describe, hspec, it, shouldBe)
import qualified Type.TypeInfer as T

main :: IO ()
main = do
    testLexerAndParser
    testEval
    testTypeInfer


testLexerAndParser :: IO ()
testLexerAndParser = hspec $ do
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


    describe "define functions" $ do
        it "normal" $ do
            (P.parse . L.lexer) "f x = x"
                `shouldBe` (Program [ Assign "f" (Lambda [PVar "x"] (Var "x")) ])
        it "normal" $ do
            (P.parse . L.lexer) "f x = x + 1"
                `shouldBe` (Program [ Assign "f" (Lambda [PVar "x"] (BinOp Add (Var "x") (Nat 1))) ])
        -- FIXME:
        -- it "const" $ do
        --     (P.parse . L.lexer) "f 1 = 1"
        --         `shouldBe` (Program [ Assign "f" (Lambda [PVar "x"] (Nat 1)) ])
        it "empty arg" $ do
            (P.parse . L.lexer) "f [] = []"
                `shouldBe` (Program [Assign "f" (Lambda [PList []] (List []))])
        -- FIXME: parse error
        -- it "const length list arg" $ do
        --     (P.parse . L.lexer) "f [x] = x"
        --         `shouldBe` (Program [Assign "f" (Lambda [PList [(PVar "x")]] (Var "x"))])
        it "cons arg" $ do
            (P.parse . L.lexer) "f (x:xs) = xs"
                `shouldBe` (Program [ Assign "f" (Lambda [PList [(PVar "x"), (PVar "xs")]] (Var "xs")) ])

    describe "call functions" $ do
        it "normal" $ do
            (P.parse . L.lexer) "f 1"
                `shouldBe` (Program [Exp (App "f" (Nat 1))])
        it "normal with parens" $ do
            (P.parse . L.lexer) "f(1)"
                `shouldBe` (Program [Exp (App "f" (Nat 1))])
        -- FIXME: parse error
        -- it "empty" $ do
        --     (P.parse . L.lexer) "f []"
        --         `shouldBe` (Program [Exp (App "f" (List []))])
        it "empty with parens" $ do
            (P.parse . L.lexer) "f([])"
                `shouldBe` (Program [Exp (App "f" (List []))])
        it "variable list" $ do
            (P.parse . L.lexer) "f [x,y]"
                `shouldBe` (Program [Exp (App "f" (List [Var "x",Var "y"]))])
        it "variable list with parens" $ do
            (P.parse . L.lexer) "f([x,y])"
                `shouldBe` (Program [Exp (App "f" (List [Var "x",Var "y"]))])
        it "num list" $ do
            (P.parse . L.lexer) "f [1,2]"
                `shouldBe` (Program [Exp (App "f" (List [Nat 1,Nat 2]))])
        it "bool list" $ do
            (P.parse . L.lexer) "f [true, false]"
                `shouldBe` (Program [Exp (App "f" (List [Bool True,Bool False]))])


testEval :: IO ()
testEval = hspec $ do
    describe "eval test" $ do
        it "add" $ do
            "10 + 42" `evalShouldBe` "52"
        it "sub" $ do
            "42 - 10" `evalShouldBe` "32"
        it "mul" $ do
            "1 * 3" `evalShouldBe` "3"
        it "bt" $ do
            "1 > 2" `evalShouldBe` "false"
        it "lt" $ do
            "1 < 2" `evalShouldBe` "true"
        it "bte" $ do
            "1 >= 2" `evalShouldBe` "false"
        it "lte" $ do
            "2 <= 2" `evalShouldBe` "true"
        it "assign" $ do
            "x = 2*3 - 4/2" `evalShouldBe` "4"
        it "if" $ do
            "if 2>1 then 1 else 2" `evalShouldBe` "1"

        it "list" $ do
            "[1,2,3]" `evalShouldBe` "[1,2,3]"
        it "list" $ do
            "[1*2, 3+4, 5]" `evalShouldBe` "[2,7,5]"

    describe "defined functions" $ do
        it "normal" $ do
            "f x = x + 1" `evalShouldBe` ""
        it "empty arg" $ do
            "f [] = []" `evalShouldBe` ""
        it "cons arg" $ do
            "f (x:xs) = xs" `evalShouldBe` ""

    describe "call functions" $ do
        it "normal" $ do
            evalCallShouldBe ["f x = x + 2", "f 1"] "3"
        -- FIXME:
        -- it "empty list" $ do
        --     evalCallShouldBe ["f [] = []", "f []"] "[]"
        it "empty list with parens" $ do
            evalCallShouldBe ["f [] = []", "f ([])"] "[]"
        it "use x of x:xs" $ do
            evalCallShouldBe ["f (x:xs) = x", "f [1,2,3]"] "1"
        it "use xs of x:xs" $ do
            evalCallShouldBe ["f (x:xs) = xs", "f [1,2,3]"] "[2,3]"

    describe "call function examples" $ do
        it "factorial function" $ do
            evalCallShouldBe ["fac n = if n > 1 then n * fac (n-1) else 1", "fac 5"] "120"
        it "head function" $ do
            evalCallShouldBe ["head [] = []", "head (x:xs) = x", "head [1,2,3]"] "1"
        it "tail function" $ do
            evalCallShouldBe ["tail (x:xs) = xs", "tail [1,2,3]"] "[2,3]"


testTypeInfer :: IO ()
testTypeInfer  = hspec $ do
    describe "type infer test" $ do
        it "CInt" $ do
            "10" `typeShouldBe` "[CInt]"
        it "CBool" $ do
            "true" `typeShouldBe` "[CBool]"

        it "add" $ do
            "1 + 2" `typeShouldBe` "[CInt]"
        it "sub" $ do
            "2 - 1" `typeShouldBe` "[CInt]"
        it "mul" $ do
            "2 * 1" `typeShouldBe` "[CInt]"
        it "div" $ do
            "2 / 1" `typeShouldBe` "[CInt]"
        it "calc" $ do
            "1 + 2 * 3 - 2" `typeShouldBe` "[CInt]"

        it "lte" $ do
            "1 <= 3" `typeShouldBe` "[CBool]"
        it "gt" $ do
            "1 > 3" `typeShouldBe` "[CBool]"
        it "if" $ do
            "if true then 3+3 else 4" `typeShouldBe` "[CInt]"
            "if true then true else false" `typeShouldBe` "[CBool]"
            "if 3>4 then true else false" `typeShouldBe` "[CBool]"
        -- it "if error" $ do
        --     "if true then true else 4" `typeShouldBe` "[CInt]"

        it "var" $ do
            "x = 3" `typeShouldBe` "[CInt]"
            "x = true" `typeShouldBe` "[CBool]"
        -- it "func" $ do
        --     "f x = x" `typeShouldBe` "[CLambda (CVar 0) (CVar 0)]"
        --     "f x = x + 1" `typeShouldBe` "[CBool (CInt) (CInt)]"


-- compiler ::


-- | Utils

evalShouldBe :: String -> String -> IO ()
evalShouldBe input result = do
    env   <- E.emptyEnv
    value <- E.runEval ((E.eval . P.parse . L.lexer) input) env
    (show value) `shouldBe` result


evalCallShouldBe :: [String] -> String -> IO ()
evalCallShouldBe inputs result = do
    env <- E.emptyEnv
    let asts = map (E.eval . P.parse . L.lexer) inputs
    evaledExps <- mapM (flip E.runEval env) asts
    (show (last evaledExps)) `shouldBe` result


typeShouldBe :: String -> String -> IO ()
typeShouldBe input result = do
    let constraint = T.infer T.emptyTIEnv ((P.parse . L.lexer) input)
    show constraint `shouldBe` result
