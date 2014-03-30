{-# LANGUAGE TupleSections #-}

module DNA
    ( count
    , nucleotideCounts
    )
where

import qualified Data.Map.Strict as M
import Text.Printf

isNucleo :: Char -> Bool
isNucleo = (`elem` "TACGU")


count :: Char -> String -> Int
count c
    | isNucleo c = length . filter (== c)
    | otherwise  = error (printf "invalid nucleotide '%c'" c)

nucleotideCounts :: String -> M.Map Char Int
nucleotideCounts = M.fromListWith (+) . map (,1) . filter isNucleo
