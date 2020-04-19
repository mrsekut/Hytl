module Type.TypeInfer where

import qualified Parser.AST                    as AST
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Map                       ( empty
                                                , fromList
                                                , Map
                                                )
import           Data.STRef                     ( newSTRef
                                                , readSTRef
                                                , STRef
                                                )

data Constraint
    = CInt
    | CBool
    deriving (Show, Eq)

type Env = Map String Constraint
type VarInfo = (Int, Map Int Constraint)


infer :: [(String, Constraint)] -> AST.Exp -> Constraint
infer env expr = runST $ do
    varInfoRef   <- newSTRef (0, empty)
    t            <- doInfer (fromList env) varInfoRef expr
    (_, varDict) <- readSTRef varInfoRef
    return $ refer t varDict


doInfer :: Env -> STRef s VarInfo -> AST.Exp -> ST s Constraint
doInfer env varInfoRef exp = case exp of
    AST.Nat  i -> pure CInt
    AST.Bool x -> pure CBool

refer :: Constraint -> Map Int Constraint -> Constraint
refer t _ = t

