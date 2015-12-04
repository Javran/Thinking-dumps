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
    , U
    , DIM0, DIM1, DIM2
    , fromListUnboxed
    , toIndex
    , fromFunction
    )

testArray :: Array U DIM2 Int
testArray = fromListUnboxed (Z :. 3 :. 5) [1..15]
