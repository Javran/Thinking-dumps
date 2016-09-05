module TypedFinal where

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

-- vz fetches the head element of the environment
vz :: (a,env) -> a
vz (vc,_) = vc

-- vs basically drops the first element and then call vp
-- to operate on the environment.
-- note: to understand this,
-- I think it's better to think how "vz", "vs vz", "vs (vs vz)" work.
vs :: (env -> a) -> (a',env) -> a
vs vp (_,envr) = vp envr

-- just boolean literal
b :: t -> env -> t
b bv _ = bv

-- function abstraction
l :: ((t1,env) -> t) -> env -> t1 -> t
l e env = \x -> e (x,env)

-- function application
a :: (env -> t1 -> t) -> (env -> t1) -> env -> t
a e1 e2 env = (e1 env) (e2 env)
