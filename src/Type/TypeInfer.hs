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
                                                , writeSTRef
                                                , STRef
                                                )
import qualified Data.Map                      as M


data Constraint
  = CInt
  | CBool
  | CVar Int
  | CLambda Constraint Constraint
  deriving (Show, Eq)

type Env = Map String Constraint
type VarInfo = (Int, Map Int Constraint)


infer :: Env -> AST.Exp -> Constraint
infer env expr = runST $ do
  varInfoRef   <- newSTRef (0, empty)
  t            <- doInfer env varInfoRef expr
  (_, varDict) <- readSTRef varInfoRef
  return $ refer t varDict


doInfer :: Env -> STRef s VarInfo -> AST.Exp -> ST s Constraint
doInfer env varInfoRef exp = case exp of
  AST.Nat  i    -> pure CInt
  AST.Bool x    -> pure CBool

  AST.Add x1 x2 -> do
    t1 <- doInfer env varInfoRef x1
    unify t1 CInt varInfoRef
    t2 <- doInfer env varInfoRef x2
    unify t2 CInt varInfoRef
    pure CInt


  AST.Var x -> case M.lookup x env of
    Just t  -> pure t
    Nothing -> fail ("not found: " ++ x)
  AST.Lambda parm e -> do
    tparm <- createVar varInfoRef
    te    <- doInfer (M.insert parm tparm env) varInfoRef e
    return $ CLambda tparm te
  AST.App f arg -> do
    argt <- doInfer env varInfoRef arg
    createVar varInfoRef


{- Utils -}

unify :: Constraint -> Constraint -> STRef s VarInfo -> ST s ()
unify (CVar i1) (CVar i2) _ | i1 == i2 = return ()
unify (CVar i1) t2        varInfoRef   = unifyVar i1 t2 varInfoRef
unify t1        (CVar i2) varInfoRef   = unifyVar i2 t1 varInfoRef
unify t1 t2 _ | t1 == t2  = return ()
              | otherwise = fail "cannot unify"


unifyVar :: Int -> Constraint -> STRef s VarInfo -> ST s ()
unifyVar index type2 varInfoRef = do
  isOccur <- occur type2 index varInfoRef
  if isOccur
    then error "occurs error"
    else do
      (nextIdx, varMap) <- readSTRef varInfoRef
      case M.lookup index varMap of
        Just vt -> unify vt type2 varInfoRef
        Nothing -> writeSTRef varInfoRef (nextIdx, M.insert index type2 varMap)


occur :: Constraint -> Int -> STRef s VarInfo -> ST s Bool
occur (CVar i) n varInfoRef
  | i == n = return True
  | otherwise = do
    (_, varMap) <- readSTRef varInfoRef
    case M.lookup i varMap of
      Just vt -> occur vt n varInfoRef
      Nothing -> return False
occur _ _ _ = return False


refer :: Constraint -> Map Int Constraint -> Constraint
refer (  CLambda p e) varMap = CLambda (refer p varMap) (refer e varMap)
refer t@(CVar v     ) varMap = case M.lookup v varMap of
  Just vt -> refer vt varMap
  Nothing -> t
refer t _ = t



createVar :: STRef s VarInfo -> ST s Constraint
createVar varInfoRef = do
  (nextIdx, varMap) <- readSTRef varInfoRef
  writeSTRef varInfoRef (nextIdx + 1, varMap)
  return $ CVar nextIdx

