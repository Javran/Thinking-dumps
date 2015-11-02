{-# LANGUAGE RecursiveDo #-}
module Deque
  ( mkDeque
  , push
  , pop
  , shift
  , unshift
  ) where

import Control.Monad.Fix
import Data.IORef

-- INVARIANT: always point to a guard element
type Deque a = IORef (Element a)

data Element a
  = Guard
      { ePrev :: IORef (Element a)
      , eNext :: IORef (Element a)
      }
  | Item
      { ePrev :: IORef (Element a)
      , eNext :: IORef (Element a)
      , eContent :: a
      }

{-
mkDeque :: IO (Deque a)
mkDeque = do
    rec r <- newIORef (Guard r r)
    return r
-}

mkDeque :: IO (Deque a)
mkDeque = mfix $ \r -> newIORef (Guard r r)

push, unshift :: Deque a -> a -> IO (Deque a)

unshift refGuard v = do
    guardNode <- readIORef refGuard
    -- guard's next pointer points to the head
    let refHead = eNext guardNode
        newNode = Item refGuard refHead v
    refNew <- newIORef newNode
    -- there are subtle issues regarding using "writeIORef":
    -- when refGuard and refHead is pointing to the same location,
    -- writeIORef will change its content and we have to retrieve
    -- the updated data (otherwise another writeIORef will overwrite
    -- our update)
    -- therefore here we choose to use "modifyIORef" in an atomic manner
    atomicModifyIORef' refGuard (\gn -> (gn { eNext = refNew },()))
    atomicModifyIORef' refHead (\hn -> (hn { ePrev = refNew }, ()))
    return refGuard

push = undefined

pop, shift :: Deque a -> IO (Maybe a)
pop = undefined
shift = undefined

visitNodes :: (a -> IO b) -> Deque a -> IO [b]
visitNodes op refGuard = do
    headNode <- readIORef refGuard
    visitNodes' (eNext headNode)
  where
    visitNodes' refCurrent =
        if refGuard == refCurrent
          then return []
          else do
            node <- readIORef refCurrent
            result <- op (eContent node)
            let refNext = eNext node
            rs <- visitNodes' refNext
            return (result:rs)
