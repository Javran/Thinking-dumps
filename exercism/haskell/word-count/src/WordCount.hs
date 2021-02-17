{-# LANGUAGE TupleSections #-}

module WordCount
  ( wordCount
  )
where

import Data.Char
import qualified Data.Map.Strict as M

wordCount :: String -> M.Map String Int
wordCount =
  M.fromListWith (+)
    . fmap ((,1) . tr)
    . words
    . fmap ((\ch -> if isAlphaNum ch || ch == apo then ch else ' ') . toLower)
  where
    apo = '\''
    tr w
      | head w == apo || last w == apo = filter (/= apo) w
      | otherwise = w
