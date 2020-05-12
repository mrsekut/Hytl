
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
    { input     = "1*3;"
    , lexered   = [TokenInt 1, TokenTimes, TokenInt 3, TokenSemicolon]
    , parsed    = Program [Exp (Mul (Nat 1) (Nat 3))]
    , contraint = C.CInt
    -- , compiled  = 3
    , evaled    = 3
    }



main :: IO ()
main = do
    t <- t1
    hspec $ describe "Parser" $ spec "test!" t



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


