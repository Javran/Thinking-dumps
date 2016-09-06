module TypedFinal2 where

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Use fst" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}

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

newtype R h a = R { unR :: h -> a }

instance Symantics R where
    int x = R $ const x
    add e1 e2 = R $ \h -> unR e1 h + unR e2 h

    z = R $ \(x,_) -> x
    s v = R $ \(_,h) -> unR v h

    lam e = R $ \h -> \x -> unR e (x,h)
    app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval :: R () a -> a
eval e = unR e ()

td1v :: Int
td1v = eval td1

td2ov :: Int -> Int
td2ov = eval (lam (app td2o (int 10)))

td3v  :: (Int -> Int) -> Int
td3v = eval td3
