{-# LANGUAGE LambdaCase #-}

module Dominoes
  ( chain
  )
where

import Data.Either
import Data.List
import Data.Maybe

type Domino = (Int, Int)

-- | A usable Domino has the desired left number.
--   if a Domino instead has a desired right number, it'll be flipped
--   in order to be usable in the results.
--   returns (<not usables>, <usables>).
usableDominoes :: Int -> [Domino] -> ([Domino], [Domino])
usableDominoes wantLeft = partitionEithers . fmap go
  where
    go p@(x, y)
      | x == wantLeft = Right p
      | y == wantLeft = Right (y, x)
      | otherwise = Left p

chain :: [Domino] -> Maybe [Domino]
chain = \case
  [] -> Just []
  x@(fstLeft, _) : xs -> listToMaybe $ do
    let search [] revChains =
          let ((_, r) : _) = revChains
           in -- final check: the rightmost should match the leftmost.
              [reverse revChains | fstLeft == r]
        search unused revChains@((_, y) : _) = do
          let (notUsables, usables) = usableDominoes y unused
          next <- usables
          let unused' = delete next usables
          search (unused' <> notUsables) (next : revChains)
        search _ [] =
          -- revChain starts with 1 and its size never decrease.
          error "unreachable"
    search xs [x]
