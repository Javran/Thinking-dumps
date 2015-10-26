{-# LANGUAGE FlexibleInstances, MonadComprehensions #-}
module SecretHandshake
  ( handshake
  ) where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Bits
import Data.Maybe

class IsHandshake a where
    -- the output "x" should have the property that:
    -- 0 <= x <= 31 (11111 in binary)
    toHandshakeCode :: a -> Int

instance IsHandshake Int where
    toHandshakeCode x
        | x >= 0 && x <= 31 = x
        | otherwise = 0

instance IsHandshake String where
    toHandshakeCode raw = case readP_to_S (binStr <* eof) raw of
        [(v,"")] -> v
        _ -> 0

binStr :: ReadP Int
binStr = convert <$> munch1 (`elem` "01")
  where
    convert :: String -> Int
    convert = foldl' (\acc i -> let v = ord i - ord '0' in v+acc*2) 0

handshake :: IsHandshake a => a -> [String]
handshake v = (if testBit hs 4 then reverse else id) intermediate
  where
    hs = toHandshakeCode v
    mayExec pos cmd = [ cmd | testBit hs pos ] :: Maybe String
    intermediate = catMaybes
                     [ mayExec 0 "wink"
                     , mayExec 1 "double blink"
                     , mayExec 2 "close your eyes"
                     , mayExec 3 "jump"
                     ]
