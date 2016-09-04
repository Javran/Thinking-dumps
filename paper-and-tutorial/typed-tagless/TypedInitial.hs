{-# LANGUAGE GADTs #-}
module TypedInitial where

-- in the initial view, we use GADTs to get control over resulting type
-- so the type tag is no longer necessary.
-- also embedding the expression in this way allows us to use GHC's typechecker
-- instead of having to write our own.
-- put it in another way: we could have use a regular datatype for the exact same purpose
-- but that also allows things that doesn't make sense to be in our language
-- (e.g. applying a boolean to another boolean).
-- and in order to address this problem, we have to deal with many other complications:
-- writing typecheckers, finding ways to distinct typechecked one from those that hasn't
-- been checked, compiler warnings about the evaluate function being partial etc.
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

eval :: env -> Exp env t -> t
eval env e = case e of
    V v -> lookp v env
    B b -> b
    L e' -> \x -> eval (x,env) e'
    A e1 e2 -> eval env e1 (eval env e2)

lookp :: Var env t -> env -> t
lookp VZ (x,_) = x
lookp (VS v) (_,env) = lookp v env


{-
-- notice here that we have a heterogeneous list
-- with type-safe lookup function

envTest :: (Bool, (String, (Int, ())))
envTest = (False,("a",(10,())))

a :: Bool
a = lookp VZ envTest

b :: String
b = lookp (VS VZ) envTest

c :: Int
c = lookp (VS (VS VZ)) envTest

-}
