module Environment (emptyEnv, Environment(..)) where

import qualified Eval           as E
import qualified Type.TypeInfer as T



data Environment = Environment {
  evalEnv :: E.Env,
  typeEnv :: T.CEnv
}

emptyEnv :: IO Environment
emptyEnv = do
  evalEnv <- E.emptyEnv
  let typeEnv = T.emptyTIEnv
  return $ Environment { evalEnv = evalEnv, typeEnv = typeEnv }
