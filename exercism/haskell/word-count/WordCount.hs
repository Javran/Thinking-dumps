{-# LANGUAGE TupleSections #-}

module WordCount
where

import qualified Data.Map as M
import Data.Char
import Control.Arrow

wordCount :: String -> M.Map String Int
wordCount = map (toLower . rmNoise) -- ^ map fusion, remains only lowers and nums
        >>> words                   -- ^ split into words
        >>> attachFreq              -- ^ attach words with frequency
        >>> M.fromListWith (+)      -- ^ build map & merge value
    where
        attachFreq = map (,1)
        rmNoise x  = if isAlphaNum x then x else ' '
