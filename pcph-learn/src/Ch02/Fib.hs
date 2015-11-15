module Ch02 where

import Control.Parallel.Strategies
import Data.Functor

fib :: Integer -> Integer
fib x = if x <= 1 then x else fib (x-1) + fib (x-2)

calc :: (Integer, Integer)
calc = runEval $ do
    a <- rpar task1
    b <- rpar task2
    void $ rseq task2
    return (a,b)
  where
    task1 = fib 30
    task2 = fib 31

main :: IO ()
main = print calc
