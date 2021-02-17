module ETL (transform) where

import Data.Char
import qualified Data.Map.Strict as M

transform :: M.Map Int String -> M.Map Char Int
transform m = M.fromList $ do
  (pt, chs) <- M.toList m
  ch <- chs
  pure (toLower ch, pt)
