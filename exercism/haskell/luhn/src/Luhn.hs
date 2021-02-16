{-# LANGUAGE ViewPatterns #-}

module Luhn
  ( checksum
  , isValid
  )
where

import Control.Monad
import Data.Char
import qualified Data.Vector.Unboxed as Vec

-- translation table for even digits counting from right.
evenTransformTable :: Vec.Vector Int
evenTransformTable = Vec.fromListN 10 $ fmap tr [0 .. 9]
  where
    tr d = if dd > 9 then dd - 9 else dd
      where
        dd = d + d

checksum :: String -> Maybe Int
checksum =
  foldM go 0
    . zip
      (cycle
         -- alternate between identity and even-digit-transformation
         [ id
         , (evenTransformTable Vec.!)
         ])
    . reverse
  where
    go acc (tr, ch) = do
      guard $ isDigit ch
      pure (acc + tr (ord ch - ord '0'))

sanitize :: String -> String
sanitize = filter (not . isSpace)

isValid :: String -> Bool
isValid (sanitize -> xs) = case xs of
  [] -> False
  [_] -> False
  _ -> case checksum xs of
    Nothing -> False
    Just v -> v `rem` 10 == 0
