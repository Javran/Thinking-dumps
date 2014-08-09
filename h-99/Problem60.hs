module Problem60
where

import Text.Printf
import Control.Applicative

{-
  n: 0
  empty

  n: 1
  o

  n: 2
    o
   /
  o

  n: 3
      o
     / \
    o   o
   /
  o

  n: 4
         o
       /   \
      o     o
     / \   /
    o   o o
   /
  o

  n: m
          o
        /  \
      /     \
    /        \
  tree(m-1) tree(m-2)


  Therefore: minNodes 0 = 0
             minNodes 1 = 1
             minNodes m = minNodes (m-1) + minNodes (m-2) + 1

   Easy to draw the conclusion that:
   minNodes m = fib (m+2) - 1
-}
minNodes :: Int -> Integer
minNodes h = fib (h+2) - 1

maxHeight :: Integer -> Int
maxHeight n = head
            . filter (\h -> minNodes h <= n
                         && n < minNodes (h+1))
            $ [0..]

fib :: Int -> Integer
fib = (fibs !!)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    mapM_ (printf "minNodes %d = %d\n" <*> minNodes) [0..10]
    mapM_ (printf "maxHeight %d = %d\n" <*> maxHeight) [0,10..100]
