{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.TypeInfer
  ( emptyTIEnv
  , infer
  , CEnv
  , doInfers
  )
where

import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (MonadState, StateT, get, put,
                                         runStateT)
import           Data.Map               (Map, empty, fromList)
import qualified Data.Map               as M
import qualified Parser.AST             as AST
import           Type.Type              (Constraint (..))



infer :: CEnv -> AST.Program -> [Constraint]
infer env expr =
  runIdentity $ runInfer (doInfers expr) (TypeState env (0, empty))


runInfer :: TI [Constraint] -> TypeState -> Identity [Constraint]
runInfer (TI a) state = do
  c <- runStateT a state
  return $ fst c


emptyTIEnv :: CEnv
emptyTIEnv = M.fromList []


type CEnv = Map String Constraint        -- (変数名, 型)
type CVarInfo = (Int, Map Int Constraint)  -- (next id, (id, 型))
data TypeState = TypeState {
  cenv_     :: CEnv,
  cvarInfo_ :: CVarInfo
}
newtype TI a = TI (StateT TypeState Identity a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState TypeState
             )



class TypeInfer a where
  doInfer :: a -> TI Constraint
  doInfers :: a -> TI [ Constraint ]


instance TypeInfer AST.Exp where
  doInfer (AST.Nat  i) = return CInt
  doInfer (AST.Bool x) = return CBool

  doInfer (AST.BinOp AST.Add x1 x2) = do
    unify CInt =<< doInfer x1
    unify CInt =<< doInfer x2
    return CInt
  doInfer (AST.BinOp AST.Sub x1 x2) = do
    unify CInt =<< doInfer x1
    unify CInt =<< doInfer x2
    return CInt
  doInfer (AST.BinOp AST.Mul x1 x2) = do
    unify CInt =<< doInfer x1
    unify CInt =<< doInfer x2
    return CInt
  doInfer (AST.BinOp AST.Div x1 x2) = do
    unify CInt =<< doInfer x1
    unify CInt =<< doInfer x2
    return CInt

  doInfer (AST.BinOp op x1 x2) = do
    unify CInt =<< doInfer x1
    unify CInt =<< doInfer x2
    return CBool

  doInfer (AST.If b t e) = do
    unify CBool =<< doInfer b
    unify CInt =<< doInfer t -- FIXME:
    unify CInt =<< doInfer e -- FIXME:
    return CBool


  doInfer (AST.Var  x) = do
    state <- get
    let env = cenv_ state
    case M.lookup x env of
      Just t  -> return t
      Nothing -> fail ("not found: " ++ x)

  doInfer (AST.Lambda parm e) = do
    tparm <- createVar
    te    <- doInfer e
    return $ CLambda tparm te

  -- スクボにメモってる
  -- doInfer (AST.App f x) = do
  --   tf <- doInfer f
  --   tx <- doInfer x
  --   undefined

instance TypeInfer AST.Stmt where
  doInfer (AST.Exp e          ) = doInfer e

  doInfer (AST.Assign name exp) = do
    state <- get
    let env = cenv_ state
    te <- doInfer exp
    put $ state { cenv_ = M.insert name te env }
    return te


instance TypeInfer AST.Program where
  doInfers (AST.Program stmt) = mapM doInfer stmt



{- Utils -}

unify :: Constraint -> Constraint -> TI ()
unify (CVar i1) (CVar i2) | i1 == i2 = return ()
unify (CVar i1) t2                   = unifyVar i1 t2
unify t1        (CVar i2)            = unifyVar i2 t1
-- unify (CLambda f1 x1) (CLambda r1 r2) = do
--   unify f1 r1
--   unify r1 r2
unify t1 t2 | t1 == t2  = return ()
            | otherwise = fail "cannot unify"


unifyVar :: Int -> Constraint -> TI ()
unifyVar index typ = do
  state <- get
  let varInfoRef = cvarInfo_ state
  isOccur <- occur index typ
  if isOccur
    then error "occurs error"
    else do
      let (nextIdx, varMap) = varInfoRef
      case M.lookup index varMap of
        Just vt -> unify vt typ
        Nothing ->
          put $ state { cvarInfo_ = (nextIdx, M.insert index typ varMap) }


occur :: Int -> Constraint -> TI Bool
occur n (CVar i)
  | i == n = return True
  | otherwise = do
    state <- get
    let (_, varMap) = cvarInfo_ state
    case M.lookup i varMap of
      Just vt -> occur n vt
      Nothing -> return False
-- occur n (CLambda f x) =
occur _ _ = return False


-- refer :: Constraint -> Map Int Constraint -> Constraint
-- refer (  CLambda p e) varMap = CLambda (refer p varMap) (refer e varMap)
-- refer t@(CVar v     ) varMap = case M.lookup v varMap of
--   Just vt -> refer vt varMap
--   Nothing -> t
-- refer t _ = t


createVar :: TI Constraint
createVar = do
  state <- get
  let (nextIdx, varMap) = cvarInfo_ state
  put $ state { cvarInfo_ = (nextIdx + 1, varMap) }
  return $ CVar nextIdx

