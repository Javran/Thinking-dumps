module TypedFinal2 where

-- typed final embedding with Int in the language.
class Symantics repr where
    int :: Int -> repr h Int
    add :: repr h Int -> repr h Int -> repr h Int

    z :: repr (a,h) a
    s :: repr h a -> repr (any,h) a
    lam :: repr (a,h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

-- expressions that have not yet get a concrete resulting type
-- the type inference does tell us the most general type
-- that those expressions could have, but we still need
-- to write down type explicitly, or otherwise GHC will try to
-- find a concrete instance on a general type, which will fail.
td1 :: Symantics repr => repr h Int
td1 = add (int 1) (int 2)

td2o :: Symantics repr => repr (Int,h) (Int -> Int)
td2o = lam (add z (s z))

-- just few reasoning:
-- 1. we definitely can apply something to the lambda
-- 2. that thing, when applied with "int 1", gives us another Int
-- 3. the lambda expects an argument of type Int -> Int, thus the type
td3 :: Symantics repr => repr h ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))
