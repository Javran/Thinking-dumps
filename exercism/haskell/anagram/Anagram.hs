module Anagram
where

import Data.Char (toLower)
import Data.List (sort)
import Data.Function (on)

anagramsFor :: String -> [String] -> [String]
anagramsFor x = filter (`equalInTermsOf` x) -- ^ (==) when both lowered and sorted
              . filter (/= x)               -- ^ exclude itself
    where
        equalInTermsOf = (==) `on` loweredAndSorted
        loweredAndSorted = sort . map toLower
