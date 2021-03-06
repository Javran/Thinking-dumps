module Main where

import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    _ <- forkIO (replicateM_ 1000 (putChar 'A'))
    replicateM_ 1000 (putChar 'B')
    -- should see interleaved "AB" according to the book...
