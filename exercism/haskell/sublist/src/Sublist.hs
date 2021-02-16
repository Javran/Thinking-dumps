module Sublist
  ( Sublist (..)
  , sublist
  )
where

import Data.List

data Sublist
  = Equal
  | Sublist
  | Superlist
  | Unequal
  deriving (Eq, Show)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys
  | xs == ys = Equal
  | xs `isSublistOf` ys = Sublist
  | ys `isSublistOf` xs = Superlist
  | otherwise = Unequal

-- | xs is a subset of ys if we can find a sublist of ys'
-- | which satisfies ys' == xs.
isSublistOf :: Eq a => [a] -> [a] -> Bool
xs `isSublistOf` ys =
  any (and . zipWith (==) xs)
    . take atMost
    $ tails ys
  where
    -- consider xs as a sliding window:
    -- from:
    -- +----+
    atMost = length ys - length xs + 1
