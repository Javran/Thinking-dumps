module Playground
  ( module Playground )
  where

-- a playground lib meant to be imported in GHCi
import Data.Array.Repa
    ( Array
    , Z(..)
    , (:.)(..)
    , U
    , DIM0, DIM1, DIM2
    , fromListUnboxed
    )

testArray :: Array U DIM1 Int
testArray = fromListUnboxed (Z :. 10) [1..10]
