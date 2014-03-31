{-# LANGUAGE TupleSections #-}

module WordCount
    ( wordCount
    )
where

import qualified Data.Map.Strict as M
import Data.Char
import Control.Arrow

-- for Data.Map.Strict, see also:
-- http://blog.ezyang.com/2011/05/an-insufficiently-lazy-map/
-- (thanks to @soapie and @etrepum)

wordCount :: String -> M.Map String Int
wordCount = map (toLower . rmNoise) -- ^ map fusion, remains only lowers and nums
        >>> words                   -- ^ split into words
        >>> attachFreq              -- ^ attach words with frequency
        >>> M.fromListWith (+)      -- ^ build map & merge value
    where
        attachFreq = map (,1)
        rmNoise x  = if isAlphaNum x then x else ' '
