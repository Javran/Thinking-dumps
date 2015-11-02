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

mkDeque :: IO (Deque a)
mkDeque = mfix $ \r -> newIORef (Guard r r)

push, unshift :: Deque a -> a -> IO (Deque a)

-- | unshift inserts elements at front
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

-- | push inserts elements at back
push refGuard v = do
    guardNode <- readIORef refGuard
    let refTail = ePrev guardNode
        newNode = Item refTail refGuard v
    refNew <- newIORef newNode
    atomicModifyIORef' refGuard (\gn -> (gn { ePrev = refNew },()))
    atomicModifyIORef' refTail (\tn -> (tn { eNext = refNew }, ()))
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
        let refXNode = eNext guardNode
        xNode <- readIORef refXNode
        let refNewHead = eNext xNode
        atomicModifyIORef' refGuard (\gn -> (gn { eNext = eNext xNode},()))
        atomicModifyIORef' refNewHead (\hn -> (hn { ePrev = ePrev xNode }, ()))
        return (Just (eContent xNode))

pop = undefined

-- a null deque contains nothing but the guard element
-- which means guard's next item is still guard itself
-- assume this deque is properly maintained, this check
-- should be sufficient
nullDeque :: Deque a -> IO Bool
nullDeque dq = readIORef dq >>= \node -> return (eNext node == dq)

-- | perform action on each non-guard nodes, debugging function
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
