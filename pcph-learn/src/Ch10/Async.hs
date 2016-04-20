module Async where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Random

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
-- from list and wait again. We can do this because TMVar has Eq typeclass,
-- using that we can turn Async into Eq as well.

-- sleep for a while and them return to fake some kind of computation
-- that takes time
delayedReturn :: Int -> a -> IO a
delayedReturn t v = threadDelay t >> pure v

main :: IO ()
main = do
    g <- newStdGen
    -- wait for 1 - 10s
    let ts = take 10 $ (* 1000000) <$> randomRs (1 :: Int,10) g
        tasks = zipWith delayedReturn ts [0 :: Int ..]
    as <- mapM async tasks
    x <- waitAny as
    print x
    ys <- mapM wait as
    print ys
    pure ()
