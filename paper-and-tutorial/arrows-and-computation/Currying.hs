module Currying where

import Control.Arrow

-- curryA gives us a way to refer to input to arrows
-- the same way as we refer to the argument to a function

{-# ANN curryA "HLint: ignore Redundant bracket" #-}
curryA :: Arrow a => a (b,c) d -> (b -> a c d)
curryA f b = arr (\c -> (b,c)) >>> f

{-

now if we want to find the inverse of curryA,
we will look for a function of the following type:

(b -> a c d) -> a (b,c) d

in the paper, we try to plug in "id :: forall a. a -> a", which
gives us a function of type:

(a c d -> a c d) -> a (a c d, c) d

-}

{-
-- TODO: figure out how to implement uncurryA
uncurryA :: (Arrow a, ArrowApply a) => (b -> a c d) -> a (b,c) d
uncurryA f = arr (\(b,c) -> let x = f b in _)
-}
