module Repl (repl) where

import           Control.Monad.Trans      (liftIO)
import           Data.List                (isPrefixOf)
import qualified Environment              as Env
import qualified Eval                     as E
import qualified Lexer.Lexer              as L
import qualified Parser.Parser            as P
import           Prelude                  hiding (read)
import qualified System.Console.Haskeline as Repl
import qualified Type.TypeInfer           as T


repl :: IO ()
repl = Repl.runInputT Repl.defaultSettings . loop =<< Env.emptyEnv


loop :: Env.Environment -> Repl.InputT IO ()
loop env = do
  input <- read
  outcome <- liftIO $ run env input
  case outcome of
    End  -> return ()
    Loop -> loop env




-- Read


read :: Repl.InputT IO Input
read = do
  maybeLine <- Repl.getInputLine "hytl> "
  case maybeLine of
    Nothing    -> return Quit
    Just chars -> return $ categorize chars


categorize :: String -> Input
categorize input
  | elem input [":q", ":quit"] = Quit
  | startWith [":t", ":type"] input = Type (eliminateCommand input)
  | startWith [":a", ":ast"] input = AST (eliminateCommand input)
  | otherwise = Run input


startWith :: [String] -> String -> Bool
startWith [] _       = False
startWith cmds input = or $ map (flip isPrefixOf input) cmds


eliminateCommand :: String -> String
eliminateCommand = drop 1 . dropWhile (>' ')




-- Run


data Input
  = Quit
  | Type String
  | Run String
  | AST String


data Outcome
  = Loop
  | End


run :: Env.Environment -> Input ->  IO Outcome
run _ Quit           = return End
run env (Type input) = do
  let ast = (P.parse . L.lexer) input
  let typ = T.infer (Env.typeEnv env) ast
  putStrLn $ show typ
  return Loop
run _ (AST input)   = do
  let ast = (P.parse . L.lexer) input
  putStrLn $ show ast
  return Loop
run env (Run input) = do
  let ast = (P.parse . L.lexer) input
  value <- E.runEval (E.eval ast) (Env.evalEnv env)
  putStrLn $ show value
  return Loop
