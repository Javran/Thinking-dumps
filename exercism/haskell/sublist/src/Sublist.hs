module Sublist
  ( sublist
  )
where

import Data.List

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ
  | xs `isSublistOf` ys = Just LT
  | ys `isSublistOf` xs = Just GT
  | otherwise = Nothing

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
