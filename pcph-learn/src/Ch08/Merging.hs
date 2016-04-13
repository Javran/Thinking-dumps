{-# LANGUAGE ScopedTypeVariables #-}
module Merging where

import GetURL

import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.List
import Data.Function
import Text.Printf
import qualified Data.ByteString as B

-- in the book `waitAny` is used to wait for one of the actions to be completed
-- and here I would like to experiment extending `waitAny` a bit
-- so that we can not only tell which action completes first, but also
-- get a full list of completed results in order.

-- note that there is another problem with `waitAny`:
-- it won't work if an empty list is given as input.
-- by doing so nothing will try to put values in `MVar`.
-- and `wait` will then end up waiting forever.

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

{-# ANN takeSameLengthAs "HLint: ignore Use zipWith" #-}
takeSameLengthAs :: [a] -> [b] -> [b]
takeSameLengthAs xs ys = map snd (zip xs ys)

-- transform a list so that each element turns into that element together
-- with rest of the elements. order in the list is not preserved.
-- example:
-- > oneAndRests [1..3]
---[(1,[2,3]),(2,[3,1]),(3,[1,2])]
oneAndRests :: [a] -> [(a,[a])]
oneAndRests [] = []
oneAndRests xs = cut (map (f . cut) . tails $ cycle xs)
  where
    cut = takeSameLengthAs xs
    f (y:ys) = (y,ys)
    f _ = error "impossible"

-- wait for any of the action to be terminated
-- either through regular termination or by excpetions
waitAny' :: forall a. [Async a] -> IO (Either SomeException a,[Async a])
waitAny' [] = error  "empty list is not allowed."
waitAny' as = do
    m <- newEmptyMVar :: IO (MVar (Either SomeException a, [Async a]))
    let forkWait (a,rests) = forkIO $ do
            r <- try (wait a)
            putMVar m (r,rests)
    mapM_ forkWait (oneAndRests as)
    readMVar m

main :: IO ()
main = do
    let sites =
            [ "http://www.bing.com"
            , "http://www.yahoo.com"
            , "http://www.wikipedia.com/wiki/Spade"
            , "http://www.wikipedia.com/wiki/Shovel"
            ]
        download url = do
            r <- getURL url
            pure (url,r)
    as <- mapM (async . download) sites
    (fix $ \loop tasks ->
       case tasks of
           [] -> putStrLn "Done."
           _ -> do
               -- whoever returns first will return not only result of itself,
               -- but also rest of the Async objects in "rests"
               -- so we can recursively reduce the length of "rests"
               -- until no task is remaining.
               -- note that these tasks are already started by using "async",
               -- so we are just sequentially looking for results but
               -- tasks are still running in parallel.
               (result, rests) <- waitAny' tasks
               case result of
                   Left e -> putStrLn ("Exception: " ++ show e)
                   Right (url,r) -> printf "Fetched %s (%d bytes)\n" url (B.length r)
               loop rests) as -- initial list of tasks
