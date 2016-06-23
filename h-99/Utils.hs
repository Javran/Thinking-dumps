{-# LANGUAGE ScopedTypeVariables #-}
module Utils where

import Data.List

{-
-- equivalent function, but it turns out to be slower.
pick :: [a] -> [(a,[a])]
pick [] = []
pick [x] = [(x,[])]
pick (x:xs) = (x,xs) : map (second (x:)) (pickAlt xs)
-}

pick :: forall a. [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split :: ([a], [a]) -> (a,[a])
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"

