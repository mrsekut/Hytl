
import           Test.Hspec
import           Lexer.Lexer                    ( lexer
                                                , Token(..)
                                                )
import           Parser.Parser                  ( parse )
import           Parser.AST                     ( Exp(..)
                                                , Stmt(..)
                                                , Program(..)
                                                )
import           Compiler                       ( CodeGen )
import qualified Type.Type                     as C
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
     , contraint :: C.Constraint
     , compiled :: Operand
     , evaled :: Integer
     }


-- m :: String -> [Token] -> Program -> C.Constriant -> Integer -> Test
-- m a b c d e =
--     Test { input = a, lexered = b, parsed = c, contraint = d, evaled = e }

-- t1 :: IO Test
-- t1 =
--     i "add"
--         $   "1*3;"
--         |>> [TokenInt 1, TokenTimes, TokenInt 3, TokenSemicolon]
--         |>> Program [Exp (Mul (Nat 1) (Nat 3))]
--         |>> C.CInt
--         |>> 3



spec :: String -> Test -> SpecWith (Arg Expectation)
spec testName t = it testName $ do
    lexer (input t) `shouldBe` lexered t
    parse (lexered t) `shouldBe` parsed t
    -- typeInfer
    -- compile
    e <- runEval (eval $ parsed t) =<< emptyEnv
    e `shouldBe` evaled t

t1 :: IO Test
t1 = return Test
    { input     = "10 + 45;"
    , lexered   = [TokenInt 10, TokenPlus, TokenInt 45, TokenSemicolon]
    , parsed    = Program [Exp (Add (Nat 10) (Nat 45))]
    , contraint = C.CInt
    -- , compiled  = 3
    , evaled    = 55
    }

t2 :: IO Test
t2 = return Test
    { input     = "42 - 10;"
    , lexered   = [TokenInt 42, TokenMinus, TokenInt 10, TokenSemicolon]
    , parsed    = Program [Exp (Sub (Nat 42) (Nat 10))]
    , contraint = C.CInt
    -- , compiled  = 3
    , evaled    = 32
    }


t3 :: IO Test
t3 = return Test
    { input     = "1 * 3;"
    , lexered   = [TokenInt 1, TokenTimes, TokenInt 3, TokenSemicolon]
    , parsed    = Program [Exp (Mul (Nat 1) (Nat 3))]
    , contraint = C.CInt
    -- , compiled  = 3
    , evaled    = 3
    }


t4 :: IO Test
t4 = return Test
    { input     = "10 + 1 * 3;"
    , lexered   = [ TokenInt 10
                  , TokenPlus
                  , TokenInt 1
                  , TokenTimes
                  , TokenInt 3
                  , TokenSemicolon
                  ]
    , parsed    = Program [Exp (Add (Nat 10) (Mul (Nat 1) (Nat 3)))]
    , contraint = C.CInt
    -- , compiled  = 3
    , evaled    = 13
    }





main :: IO ()
main = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    hspec $ describe "Parser" $ do
        spec "test!" t1'
        spec "test!" t2'
        spec "test!" t3'
        spec "test!" t4'



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


