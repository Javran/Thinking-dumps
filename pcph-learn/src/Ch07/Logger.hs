module Main where

import Control.Concurrent
import Control.Monad
import System.IO

data Logger = Logger (MVar LogCommand)

data LogCommand
  = Message String -- message to be logged
  | Stop (MVar ()) -- a mvar to be "notified" when the logger is terminating

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
        cmd <- takeMVar m
        case cmd of
            Message msg ->
                putStrLn ("Log: " ++ msg) >> loop
            Stop s ->
                putStrLn "Logger: stopping" >> putMVar s ()

initLogger :: IO Logger
initLogger = do
    m <- newEmptyMVar
    let l = Logger m
    _ <- forkIO (logger l)
    pure l

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) msg = putMVar m (Message msg)

logStop :: Logger -> IO ()
logStop (Logger m) = do
    s <- newEmptyMVar
    putMVar m (Stop s)
    takeMVar s
    putStrLn "Logger has been stopped."

main :: IO ()
main = do
    l <- initLogger
    logMessage l "excited!"
    logMessage l "+1s"
    logStop l
