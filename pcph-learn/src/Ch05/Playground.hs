module Playground
  ( module Playground )
  where

-- a playground lib meant to be imported in GHCi
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Index
    ( (:.)
    , DIM0, DIM1, DIM2
    )

import Data.Array.Repa
    ( Array
    , Z
    , U
    , fromListUnboxed
    )

testArray :: Array U DIM1 Int
testArray = fromListUnboxed (Z :. 10) [1..10]
