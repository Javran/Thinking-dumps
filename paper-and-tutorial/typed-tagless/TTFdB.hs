{-# LANGUAGE NoMonomorphismRestriction #-}
module TTFdB where

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use fst" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

-- Typed Tagless-Final, de Bruijn indices

class Semantics repr where
    int :: Int -> repr h Int
    add :: repr h Int -> repr h Int -> repr h Int

    z :: repr (a,h) a
    s :: repr h a -> repr (any,h) a -- "any" for the unrelated part of "h"
    lam :: repr (a,h) b -> repr h (a -> b)
    app :: repr h (a -> b) -> repr h a -> repr h b

td1 :: Semantics repr => repr h Int
-- 1 + 1
td1 = add (int 1) (int 2)

td2 :: Semantics repr => repr h (Int -> Int)
-- \x -> x + x
td2 = lam (add z z)

td2o :: Semantics repr => repr (Int,h) (Int -> Int)
-- \x -> x + y (y is free in this case)
td2o = lam (add z (s z))

td3 :: Semantics repr => repr h ((Int -> Int) -> Int)
-- \x -> (x+1) + 2
td3 = lam (add (app z (int 1)) (int 2))

newtype R h a = R { unR :: h -> a }

instance Semantics R where
    int x = R $ \_ -> x
    add e1 e2 = R $ \h -> unR e1 h + unR e2 h

    z = R $ \(x,_) -> x
    s v = R $ \(_,h) -> unR v h
    lam e = R $ \h -> \x -> unR e (x,h)
    app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

eval :: R () a -> a
eval e = unR e ()

newtype S h a = S { unS :: Int -> String }

instance Semantics S where
    int x = S $ \_ -> show x
    add e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"
    z = S $ \h -> "x" ++ show (h-1)
    s v = S $ \h -> unS v (h-1)
    lam e = S $ \h ->
        -- notice the fact here that we indeed bind
        -- the pretty-printed variable to "x",
        -- but we are not really passing "x" to another function
        -- therefore the context carried is just the number.
        let x = "x" ++ show h
        in "(\\" ++ x ++ " -> " ++ unS e (h+1) ++ ")"
    app e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S () a -> String
view e = unS e 0
