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

td2ov :: Int -> (Int -> Int)
-- the outter "lam" is driven by typechecker:
-- 1. simply "eval td2o" won't do, as td2o needs proper environment
--    but the evaluator expects an empty one.
-- 2. picking "lam" allows us to "flatten" the environment from (a,h) to h,
--    and we instead need to apply it to something in order to plug in that value
-- 3. the original expression gives a value of type "Int -> Int",
--    so putting a "lam" to wrap it inside gives us a value of "Int -> (Int -> Int)",
--    in which the first value is used to provide the expected environment
--    for the original expression to use.
td2ov = eval (lam td2o)

-- instead of modifying the original expression, we choose to provide
-- a proper environment for the expression.
-- operationally this is the same as "td2ov", but taking a different view
-- of how we view things.
td2ov' :: Int -> (Int -> Int)
td2ov' x = unR td2o (x,())

td3v  :: (Int -> Int) -> Int
td3v = eval td3

newtype S h a = S { unS :: Int -> String }

instance Symantics S where
    int x = S $ const $ show x
    add e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"
    z = S $ \h -> "x" ++ show (h-1)
    s v = S $ \h -> unS v (h-1)
    lam e = S $ \h ->
        let x = "x" ++ show h
        in "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
    app e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S () a -> String
view e = unS e 0

td1v2 :: String
td1v2 = view td1

-- unlike others, "td2o" is expecting
-- a different "shape" of the environment,
-- so "view" would not work properly on it.
-- instead of "0", we need "1" in order to give
-- the correct number to variables.
td2ov2 :: String
td2ov2 = unS td2o 1

td3v2 :: String
td3v2 = view td3
