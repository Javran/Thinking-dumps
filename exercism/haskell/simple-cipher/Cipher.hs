module Cipher
  ( caesarEncode
  , caesarDecode
  , caesarEncodeRandom
  ) where

import Control.Monad.Random
import Control.Monad

caesarEncode :: String -> String -> String
caesarEncode = undefined

caesarDecode :: String -> String -> String
caesarDecode = undefined

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom pt = do
    -- key length: at least 100 chars
    kl <- getRandomR (100,200)
    key <- replicateM kl (getRandomR ('a','z'))
    return (key,caesarEncode key pt)
