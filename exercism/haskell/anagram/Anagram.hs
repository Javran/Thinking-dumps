module Anagram
    ( anagramsFor
    )
where

import Data.Char (toLower)
import Data.List (sort)
import Control.Arrow ((&&&), (>>>))

anagramsFor :: String -> [String] -> [String]
anagramsFor x = map (id &&& loweredAndSorted) -- split data into: <origin, imaged>
            >>> filter (/= (x,x'))            -- exclude x itself
            >>> filter ((== x') . snd)        -- imaged data == imaged x
            >>> map fst                       -- return the origin tied to it
    where
        loweredAndSorted = sort . map toLower
        x' = loweredAndSorted x
