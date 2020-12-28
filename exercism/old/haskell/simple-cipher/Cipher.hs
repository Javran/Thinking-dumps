module Cipher
  ( caesarEncode
  , caesarDecode
  , caesarEncodeRandom
  ) where

import Control.Monad.Random
import Control.Monad
import Data.Char

-- (internal) 'shiftChar ch step',
-- 'ch' should be lowercase a-z
shiftChar :: Char -> Int -> Char
shiftChar ch step = ['a' .. 'z'] !! pos
  where
    stepNorm = step `mod` 26
    pos = (asOffset ch + stepNorm) `rem` 26

asOffset :: Char -> Int
asOffset ch = ord ch - ord 'a'

{-# ANN applyOffsets "HLint: ignore Use String" #-}
applyOffsets :: [Int] -> [Char] -> [Char]
applyOffsets ks = zipWith (flip shiftChar) (cycle ks)

caesarEncode :: String -> String -> String
caesarEncode ks = applyOffsets (map asOffset ks)

caesarDecode :: String -> String -> String
caesarDecode ks = applyOffsets (map (\x -> -asOffset x) ks)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom pt = do
    -- key length: at least 100 chars
    kl <- getRandomR (100,200)
    key <- replicateM kl (getRandomR ('a','z'))
    return (key,caesarEncode key pt)
