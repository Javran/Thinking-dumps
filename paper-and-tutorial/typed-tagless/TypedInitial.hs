{-# LANGUAGE GADTs #-}
module TypedInitial where

data Exp env t where
    B :: Bool -> Exp env Bool
    V :: Var env t -> Exp env t
    L :: Exp (a,env) b -> Exp env (a -> b)
    A :: Exp env (a -> b) -> Exp env a -> Exp env b

data Var env t where
    VZ :: Var (t,env) t
    VS :: Var env t -> Var (a,env) t

ti1 :: Exp env Bool
ti1 = A (L (V VZ)) (B True)
