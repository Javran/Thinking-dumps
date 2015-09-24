{-# LANGUAGE ScopedTypeVariables #-}
module Octal
  ( showOct
  , readOct
  ) where

import Data.Char
import Data.Maybe

-- | convert from @Integral@ to octal digits
octDigits :: Integral a => a -> [Int]
octDigits 0 = [0]
octDigits v = map fromIntegral (reverse (octDigitsR v))
  where
    octDigitsR 0 = []
    octDigitsR n = r : octDigitsR q
      where
        (q,r) = n `quotRem` 8

-- | show @Integral@ as octal number
showOct :: Integral a => a -> String
showOct = map (chr . (+ ord '0')) . octDigits

-- | read @Integral@ from octal digit strings
readOct :: forall a. Integral a => String -> a
readOct xs = fromMaybe 0 $
               -- parse then accumulate digits
               compute <$> mapM toDigit xs
  where
    compute :: [Int] -> a
    compute = foldl (\acc i -> acc*8 + fromIntegral i) 0
    toDigit x =
        if isDigit x
          then Just (ord x - ord '0')
          else Nothing
