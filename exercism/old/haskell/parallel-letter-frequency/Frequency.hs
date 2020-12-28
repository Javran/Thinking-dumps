{-# LANGUAGE TupleSections #-}
module Frequency
  ( frequency
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Data.Char
import Control.Parallel.Strategies

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n =
      M.unionsWith (+)
    . withStrategy (parBuffer n rdeepseq)
    . map frequency'

frequency' :: T.Text -> M.Map Char Int
frequency' =
      M.fromListWith (+)
    . map ((,1) . toLower)
    . filter isLetter
    . T.unpack
