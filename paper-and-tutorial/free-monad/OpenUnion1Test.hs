{-# LANGUAGE FlexibleContexts #-}
module OpenUnion1Test where

import OpenUnion1

import Control.Monad.State

test1 :: Member [] r => Union r Int
test1 = inj [1]

test2 :: Member (State Int) r => Union r Int
test2 = inj (get :: State Int Int)

{-
-- not sure what is the correct use of "prj"
test3 = prj (inj (get :: State Int Int))
-}
