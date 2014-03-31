{-# LANGUAGE TupleSections #-}

module DNA
    ( count
    , nucleotideCounts
    )
where

import qualified Data.Map.Strict as M
import Text.Printf

-- | test if the given char is nucleotide
isNucleo :: Char -> Bool
isNucleo = (`elem` "TACGU")

-- | count occurence of a given nucleotide
--   WARNING: will raise an error
--   if the given char is not a valid nucleotide
count :: Char -> String -> Int
count c
    | isNucleo c = length . filter (== c)
    | otherwise  = error (printf "invalid nucleotide '%c'" c)

-- | count occurences of all nucleotides given a DNA string
nucleotideCounts :: String -> M.Map Char Int
nucleotideCounts xs = (M.fromList . map queryToResult) "TACG"
    where
        -- | tie query (Char) with its result
        queryToResult x = (x, M.findWithDefault 0 x realMap)
        realMap = ( M.fromListWith (+) -- 3. accumulate
                  . map (,1)           -- 2. tag with occurrence
                  . filter isNucleo    -- 1. only interested in nucleotide
                  ) xs
