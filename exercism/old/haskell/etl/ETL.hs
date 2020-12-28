{-# LANGUAGE TupleSections #-}
module ETL
    ( transform )
where

import Control.Arrow
import Data.Char
import Data.Tuple

import qualified Data.Map.Strict as M

transform :: M.Map Int [String] -> M.Map String Int
          -- work on list
transform =   M.toList
          -- fusion: swap key-value and split pairs
          >>> concatMap (splitPair . swap)
          -- convert key to lowercase
          >>> map (first strToLower)
          -- back to Map
          >>> M.fromList
    where
        strToLower = map toLower
        splitPair (as,b) = map (,b) as
