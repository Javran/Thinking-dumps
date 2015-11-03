{-# LANGUAGE TemplateHaskell #-}
module Deque
  ( mkDeque
  , push
  , pop
  , shift
  , unshift
    -- debugging function:
  , visitNodes
  ) where

import Control.Monad.Fix
import Data.IORef
import Control.Lens

-- TODO: thread-safe

-- INVARIANT: always point to a guard element
type Deque a = IORef (Element a)

data Element a
  = Guard
      { _ePrev :: IORef (Element a)
      , _eNext :: IORef (Element a)
      }
  | Item
      { _ePrev :: IORef (Element a)
      , _eNext :: IORef (Element a)
      , _eContent :: a
      }

makeLenses ''Element

mkDeque :: IO (Deque a)
mkDeque = mfix $ \r -> newIORef (Guard r r)

push, unshift :: Deque a -> a -> IO (Deque a)

-- | unshift inserts elements at front
unshift refGuard v = do
    guardNode <- readIORef refGuard
    -- guard's next pointer points to the head
    let refHead = _eNext guardNode
        newNode = Item refGuard refHead v
    refNew <- newIORef newNode
    -- there are subtle issues regarding using "writeIORef":
    -- when refGuard and refHead is pointing to the same location,
    -- writeIORef will change its content and we have to retrieve
    -- the updated data (otherwise another writeIORef will overwrite
    -- our update)
    -- therefore here we choose to use "modifyIORef" in an atomic manner
    atomicModifyIORef' refGuard (\gn -> (gn { _eNext = refNew },()))
    atomicModifyIORef' refHead (\hn -> (hn { _ePrev = refNew }, ()))
    return refGuard

-- | push inserts elements at back
push refGuard v = do
    guardNode <- readIORef refGuard
    let refTail = _ePrev guardNode
        newNode = Item refTail refGuard v
    refNew <- newIORef newNode
    atomicModifyIORef' refGuard (\gn -> (gn { _ePrev = refNew },()))
    atomicModifyIORef' refTail (\tn -> (tn { _eNext = refNew }, ()))
    return refGuard

pop, shift :: Deque a -> IO (Maybe a)

shift dq = do
    -- for readability
    let refGuard = dq
    b <- nullDeque dq
    if b
      then return Nothing
      else do
        guardNode <- readIORef refGuard
        -- XNode is the node to be removed
        let refXNode = _eNext guardNode
        xNode <- readIORef refXNode
        let refNewHead = _eNext xNode
        atomicModifyIORef' refGuard (\gn -> (gn { _eNext = _eNext xNode},()))
        atomicModifyIORef' refNewHead (\hn -> (hn { _ePrev = _ePrev xNode }, ()))
        return (Just (_eContent xNode))

pop dq = do
    let refGuard = dq
    b <- nullDeque dq
    if b
      then return Nothing
      else do
        guardNode <- readIORef refGuard
        -- XNode is the node to be removed
        let refXNode = _ePrev guardNode
        xNode <- readIORef refXNode
        let refNewTail = _ePrev xNode
        atomicModifyIORef' refGuard (\gn -> (gn { _ePrev = _ePrev xNode},()))
        atomicModifyIORef' refNewTail (\tn -> (tn { _eNext = _eNext xNode }, ()))
        return (Just (_eContent xNode))

-- a null deque contains nothing but the guard element
-- which means guard's next item is still guard itself
-- assume this deque is properly maintained, this check
-- should be sufficient
nullDeque :: Deque a -> IO Bool
nullDeque dq = readIORef dq >>= \node -> return (_eNext node == dq)

-- | perform action on each non-guard nodes, debugging function
visitNodes :: (a -> IO b) -> Deque a -> IO [b]
visitNodes action refGuard = do
    headNode <- readIORef refGuard
    visitNodes' (_eNext headNode)
  where
    visitNodes' refCurrent =
        if refGuard == refCurrent
          then return []
          else do
            node <- readIORef refCurrent
            result <- action (_eContent node)
            let refNext = _eNext node
            rs <- visitNodes' refNext
            return (result:rs)
