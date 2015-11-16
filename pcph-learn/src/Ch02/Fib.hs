module Ch02 where

import Data.Functor
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

test1, test2, test3, test4 :: Eval (Integer, Integer)

test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)

test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)

test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  void $ rseq x
  return (x,y)

test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  void $ rseq x
  void $ rseq y
  return (x,y)

printTimeSince :: UTCTime -> IO ()
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

main :: IO ()
main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  -- first timestamp: when computation returns
  printTimeSince t0
  print r
  -- second timestamp: when we actually have `r` available
  printTimeSince t0
