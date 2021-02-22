{-# LANGUAGE ViewPatterns #-}

module Isogram
  ( isIsogram
  )
where

import Data.Char

isIsogram :: String -> Bool
isIsogram = isIsogram' . concatMap norm
  where
    norm ch = [toUpper ch | isAlpha ch]
    isIsogram' [] = True
    isIsogram' (y : ys) = notElem y ys && isIsogram' ys
