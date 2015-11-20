module Fib where

{-
copied from Ch02, for verifying what rparWith does:

* `rparWith` can be used to guaranteed that the evaluation
  is not blocking anything, even if the inner strategy is rseq
  (TODO: see NOTES.md for more explanations)

-}

import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- this time test1, test2, test3 should all behavior in a similar way
-- because `rparWith` is around
test1, test2, test3 :: Eval (Integer, Integer)

test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)

test2 = do
  x <- rparWith rseq (fib 36)
  y <- rpar (fib 35)
  return (x,y)

test3 = do
  x <- rparWith rseq (fib 36)
  y <- rparWith rseq (fib 35)
  return (x,y)

printTimeSince :: UTCTime -> IO ()
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

main :: IO ()
main = do
  [n] <- getArgs
  let test = [test1,test2,test3] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  -- first timestamp: when computation returns
  printTimeSince t0
  print r
  -- second timestamp: when we actually have `r` available
  printTimeSince t0
