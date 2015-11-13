{-# LANGUAGE TupleSections #-}
module Frequency
  ( frequency
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Char
import Data.Monoid

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n = M.unionsWith (+)
              . map (M.fromListWith (+)
                     . map ((,1) . toLower)
                     . filter isLetter
                     . T.unpack)
