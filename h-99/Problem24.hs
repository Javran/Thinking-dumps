module Problem24 where

import Control.Monad.Random

import Problem22 (range)
import Problem23 (rndSelectNoRep)

diffSelect :: MonadRandom m => Int -> Int -> m [Int]
diffSelect n hi = rndSelectNoRep (range 1 hi) n

main :: IO ()
main = diffSelect 6 49 >>= print
