module Frequency
  ( frequency
  )
where

import Control.Parallel.Strategies
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency n =
  M.unionsWith (+)
    . withStrategy (parBuffer n rdeepseq)
    . map frequency'

frequency' :: T.Text -> M.Map Char Int
frequency' =
  M.fromListWith (+)
    . concatMap (\ch -> [(ch, 1) | isLetter ch])
    . T.unpack
