{-# LANGUAGE FlexibleContexts #-}
module Playground
  ( module Playground )
  where

-- a playground lib meant to be imported in GHCi

import qualified Data.Array.Repa as Repa
import Data.Array.Repa
    ( Array
    , Z(..)
    , (:.)(..)
    , (!)
    , U, D
    , DIM0, DIM1, DIM2
    , fromListUnboxed
    , toIndex
    , fromFunction
    , computeS
    )

testArray :: Array U DIM2 Int
testArray = fromListUnboxed (Z :. 3 :. 5) [1..15]

testArray2 :: Array D DIM2 Int
testArray2 = Repa.map succ testArray

testArray3 :: Array U DIM2 Int
testArray3 = computeS testArray2
