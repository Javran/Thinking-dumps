module Async where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

data Async a = Async ThreadId (TMVar (Either SomeException a))

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

main :: IO ()
main = pure ()
