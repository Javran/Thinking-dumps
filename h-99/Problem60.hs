module Problem60
where

import Text.Printf
import Control.Applicative
import Control.Monad

import BinaryTree

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

maxNodes :: Int -> Integer
maxNodes h = 2^h-1

maxHeight :: Integer -> Int
maxHeight n = head
            . filter (\h -> minNodes h <= n
                         && n < minNodes (h+1))
            $ [0..]

minHeight :: Integer -> Int
minHeight n = head
            . filter (\h -> maxNodes h <= n
                         && n < maxNodes (h+1))
            $ [0..]

fib :: Int -> Integer
fib = (fibs !!)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes v c = concatMap (genTree c') [minHeight c' .. maxHeight c']
    where
        c' = fromIntegral c
        -- generate tree of height h with n nodes
        -- n and h in genTree is assumed to have the following property:
        -- minNodes h <= n <= maxNodes h
        genTree 0 0 = [Empty] -- n must be 0 if h = 0
        genTree 1 1 = [leaf v] -- n must be 1 if h = 1
        genTree _ h | h <= 1 = []
        genTree n h = do
            -- preserve the property of "almost balanced"
            (lHeight, rHeight) <- [ (h-2,h-1)
                                  , (h-1,h-1)
                                  , (h-1,h-2) ]
            lNodes <- [minNodes lHeight..maxNodes lHeight]
            let rNodes = n - lNodes - 1
            guard $ minNodes rHeight <= rNodes && rNodes <= maxNodes rHeight
            Branch v <$> genTree lNodes lHeight
                     <*> genTree rNodes rHeight

main :: IO ()
main = do
    mapM_ (printf "minNodes %d = %d\n" <*> minNodes) [0..10]
    mapM_ (printf "maxHeight %d = %d\n" <*> maxHeight) [0,10..100]
    print . length $ hbalTreeNodes 'x' 15
    mapM_ (print . hbalTreeNodes 'x') [0..3]
