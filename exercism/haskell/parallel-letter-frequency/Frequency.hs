{-# LANGUAGE TupleSections #-}
module Frequency
  ( frequency
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Char
import Control.Parallel.Strategies

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n ts = M.unionsWith (+) $ (map (M.fromListWith (+) . map ((,1) . toLower) . filter isLetter . T.unpack) ts `using` parBuffer n rdeepseq)
