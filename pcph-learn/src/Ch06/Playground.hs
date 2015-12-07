{-# LANGUAGE FlexibleContexts #-}
module Playground
  ( module Playground )
  where

-- a playground lib meant to be imported in GHCi

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Control.Arrow

testArray :: Vector Int
testArray = fromList (Z :. 10) [1..10]

testArray2 :: Array DIM2 Int
testArray2 = fromList (Z :. 3 :. 4) [1..]

testArrayTup :: Array DIM2 (Int, Int)
testArrayTup = fromList (Z :. 2 :. 3) (Prelude.zip [1..] [1..])

-- can (+ 1) be arbitrary operation? "succ" doesn't seem to work
arrResult :: Array DIM2 Int
arrResult = run (A.map (+ 1) (use testArray2))
