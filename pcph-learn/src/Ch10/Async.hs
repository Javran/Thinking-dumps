{-# LANGUAGE TupleSections #-}
module Async where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.IntMap as IM
import System.Random
import Data.Function
import Control.Monad

data Async a = Async ThreadId (TMVar (Either SomeException a)) deriving (Eq)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyTMVarIO
    -- when action is completed, forkFinally's second argument
    -- is called with the result
    -- and we put the result into TMVar
    t <- forkFinally action (atomically . putTMVar var)
    pure (Async t var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
    r <- waitCatchSTM a
    case r of
        Left e -> throwSTM e
        Right v -> pure v

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b =
    atomically $ (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

{-# ANN waitAny "HLint: ignore Use ." #-}
waitAny :: [Async a] -> IO a
waitAny = atomically . foldr orElse retry . map waitSTM

wait :: Async a -> IO a
wait = atomically . waitSTM

-- like before, here I'd like to build up something that
-- not only knows which computation is done first,
-- but also gives a whole list of computations in their order of completion.

-- unlike my previous approach, here I will not transform a list into (<element>,<rest>).
-- instead, when we gets one result back from a list of Asyncs, we'll remove that Async
-- from list and wait again. This might be difficult in general, as
-- by using `orElse` we lose track of the source.
-- so when assigning tasks, we also assign the task with an id.

-- sleep for a while and them return to fake some kind of computation
-- that takes time
delayedReturn :: Int -> a -> IO a
delayedReturn t v = threadDelay t >> pure v

main :: IO ()
main = do
    g <- newStdGen
    -- wait for 1 - 5s
    let ts = take 10 $ (* 1000000) <$> randomRs (1 :: Int,5) g
        tasks = zipWith (\tId time -> (tId,delayedReturn time tId)) [0 :: Int ..] ts
        taskDict = IM.fromList tasks

    as <- forM (IM.toList taskDict) $ \(tId, action) -> do
        r <- async action
        pure (tId, r)

    let taskDictA = IM.fromList as :: IM.IntMap (Async Int)
    result <- fix (\loop accumulated dict ->
         if IM.null dict
           then pure (reverse accumulated)
           else do
             let waitList = IM.elems dict
             r <- waitAny waitList
             loop (r:accumulated) (IM.delete r dict)
         ) [] taskDictA
    print result
