module Anagram
    ( anagramsFor
    )
where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter (== x) -- ^ equal to `x`
              . filter (/= x) -- ^ exclude itself
              . sortedImageOf -- ^ to lowercase and sort
    where
        loweredAndSorted = sort . map toLower
        -- convert and keep the lowered
        -- and sorted result
        sortedImageOf = map loweredAndSorted
