module Merging where

import GetURL

import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.List

-- in the book `waitAny` is used to wait for one of the actions to be completed
-- and here I would like to experiment extending `waitAny` a bit
-- so that we can not only tell which action completes first, but also
-- get a full list of completed results in order.

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    _ <- forkIO (try action >>= putMVar var)
    pure (Async var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a
wait a = do
    r <- waitCatch a
    case r of
        Left e -> throwIO e
        Right v -> pure v

{-# ANN module "HLint: ignore Use zipWith" #-}
takeSameLengthAs :: [a] -> [b] -> [b]
takeSameLengthAs xs ys = map snd (zip xs ys)

-- transform a list so that each element turns into that element together
-- with rest of the elements. order in the list is not preserved.
oneAndRests :: [a] -> [(a,[a])]
oneAndRests [] = []
oneAndRests xs = cut (map (f . cut) . tails $ cycle xs)
  where
    cut = takeSameLengthAs xs
    f (y:ys) = (y,ys)
    f _ = error "impossible"

main :: IO ()
main = pure ()
