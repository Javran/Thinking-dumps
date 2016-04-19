module TMVar where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Concurrent

newtype TMVar a = TMVar (TVar (Maybe a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
    m <- readTVar t
    case m of
        Nothing -> retry
        Just a -> do
            -- because the operation runs in
            -- "atomic" environment, we don't need to
            -- worry about other threads.
            writeTVar t Nothing
            pure a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
    m <- readTVar t
    case m of
        Nothing -> writeTVar t (Just a)
        Just _ -> retry

addSTM :: TMVar Int -> TMVar Int -> TMVar Int -> STM ()
addSTM ta tb tresult = do
    a <- takeTMVar ta
    b <- takeTMVar tb
    putTMVar tresult (a + b)

main :: IO ()
main = do
    ref1 <- atomically $ newEmptyTMVar
    ref2 <- atomically $ newEmptyTMVar
    ref3 <- atomically $ newEmptyTMVar
    forkIO $ do
        atomically $ addSTM ref1 ref2 ref3
    forkIO $ atomically $ putTMVar ref2 100
    forkIO $ atomically $ putTMVar ref1 200

    -- this works. there seems to be an allowed timeout,
    -- I guess if nothing happens to change the situation
    -- after certain time, an exception will be thrown.
    result <- atomically (takeTMVar ref3)
    print result
