module Repl
  ( astRepl
  , evalRepl
  )
where


import           Control.Monad  (unless)
import           Eval           (Env, emptyEnv, eval, runEval)
import           Lexer.Lexer
import           Parser.Parser
import           System.IO
import           Type.TypeInfer (CEnv, emptyTIEnv, infer)


{- AST mode -}

astRepl :: IO ()
astRepl = repl $ show . parse . lexer
 where
  repl :: (String -> String) -> IO ()
  repl eval = do
    input <- read_
    unless (input == ":quit") $ print_ (eval input) >> repl eval




{- Eval mode -}

evalRepl :: IO ()
evalRepl = do
  inprEnv <- emptyEnv
  replIO inprEnv emptyTIEnv
 where
  replIO :: Env -> CEnv -> IO ()
  replIO env tiEnv = do
    input <- read_
    let ast = (parse . lexer) input
    value <- runEval (eval ast) env

    let typ = infer tiEnv ast
    f input value env tiEnv typ

  f input value env tiEnv typ
    | input == ":quit" || input == ":q" = pure ()
    | input == ":type" || input == ":t" = do
      print_ $ show typ
      replIO env tiEnv
    | otherwise = do
      print_ $ show value
      replIO env tiEnv



{- Utils -}

read_ :: IO String
read_ = do
  putStr "hytl> "
  hFlush stdout
  getLine

print_ :: String -> IO ()
print_ = putStrLn
