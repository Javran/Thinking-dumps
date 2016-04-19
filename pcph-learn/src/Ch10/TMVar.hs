module TMVar where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

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
