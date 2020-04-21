{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.TypeInfer where

import           Type.Type                      ( Constraint(..) )
import qualified Parser.AST                    as AST
import           Control.Monad.State            ( StateT
                                                , runStateT
                                                , put
                                                , get
                                                , MonadState
                                                )
import           Control.Monad.Identity         ( Identity
                                                , runIdentity
                                                )
import           Data.Map                       ( empty
                                                , fromList
                                                , Map
                                                )
import qualified Data.Map                      as M



infer :: Env -> AST.Exp -> Constraint
infer env expr = runIdentity $ runInfer
  (do
    t     <- doInfer expr
    state <- get
    let (_, varDict) = typeList_ state
    return $ refer t varDict
  )
  (TypeState env (0, empty))


runInfer :: TI Constraint -> TypeState -> Identity Constraint
runInfer ti state = do
  let (TI a) = ti
  let b      = runStateT a state
  (c, _) <- b
  return c




type Env = Map String Constraint
type VarInfo = (Int, Map Int Constraint)
data TypeState = TypeState {
  env_ :: Env,
  typeList_ :: VarInfo
}
newtype TI a = TI (StateT TypeState Identity a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState TypeState
             )



class TypeInfer a where
  doInfer :: AST.Exp -> TI a


instance TypeInfer Constraint where
  doInfer (AST.Nat  i   ) = return CInt
  doInfer (AST.Bool x   ) = return CBool

  doInfer (AST.Add x1 x2) = do
    t1 <- doInfer x1
    unify t1 CInt
    t2 <- doInfer x2
    unify t2 CInt
    return CInt

  doInfer (AST.Var x) = do
    state <- get
    let env = env_ state
    case M.lookup x env of
      Just t  -> return t
      Nothing -> fail ("not found: " ++ x)

  doInfer (AST.Lambda parm e) = do
    tparm <- createVar
    te    <- doInfer e
    return $ CLambda tparm te

  doInfer (AST.App f arg) = createVar






{- Utils -}

unify :: Constraint -> Constraint -> TI ()
unify (CVar i1) (CVar i2) | i1 == i2 = return ()
unify (CVar i1) t2                   = unifyVar i1 t2
unify t1        (CVar i2)            = unifyVar i2 t1
unify t1 t2 | t1 == t2  = return ()
            | otherwise = fail "cannot unify"


unifyVar :: Int -> Constraint -> TI ()
unifyVar index typ = do
  state <- get
  let varInfoRef = typeList_ state
  isOccur <- occur typ index varInfoRef
  if isOccur
    then error "occurs error"
    else do
      let (nextIdx, varMap) = varInfoRef
      case M.lookup index varMap of
        Just vt -> unify vt typ
        Nothing ->
          put $ state { typeList_ = (nextIdx, M.insert index typ varMap) }


occur :: Constraint -> Int -> VarInfo -> TI Bool
occur (CVar i) n varInfoRef
  | i == n = return True
  | otherwise = do
    let (_, varMap) = varInfoRef
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


createVar :: TI Constraint
createVar = do
  state <- get
  let (nextIdx, varMap) = typeList_ state
  put $ state { typeList_ = (nextIdx + 1, varMap) }
  return $ CVar nextIdx

