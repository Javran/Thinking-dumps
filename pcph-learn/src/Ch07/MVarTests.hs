module Main where

import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    m <- newEmptyMVar
    _ <- forkIO $ do
        putStrLn "Child: feeding 'x'"
        putMVar m 'x'
        putStrLn "Child: feeding 'y'"
        putMVar m 'y'
    r <- takeMVar m
    print r
    r' <- takeMVar m
    print r'
    -- might see interleaving outputs,
    -- because child thread is still running while
    -- the main thread printing out the last char received
