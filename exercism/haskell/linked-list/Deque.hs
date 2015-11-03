{-# LANGUAGE TemplateHaskell, RankNTypes, TupleSections #-}
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

dqInsert :: Simple Lens (Element a) (IORef (Element a))
         -> Simple Lens (Element a) (IORef (Element a))
         -> IORef (Element a) -> a -> IO ()
dqInsert dir revDir refCurrent v = do
    curNode <- readIORef refCurrent
    -- guard's next pointer points to the head
    let refNext = curNode ^. dir
        -- note that because of the level of abstraction we have,
        -- we are not able to figure out how to create this newNode
        -- (since it requires properly assigne two refs)
        -- so we just leave it blank ("undefined") and fill these two fields
        -- immediately. thus "dir" and "revDir" are also responsible for
        -- Item's creation
        newNode = Item undefined undefined v
                    & dir .~ refNext
                    & revDir .~ refCurrent
--            Item refCurrent refNext v
    refNew <- newIORef newNode
    -- there are subtle issues regarding using "writeIORef":
    -- when refGuard and refHead is pointing to the same location,
    -- writeIORef will change its content and we have to retrieve
    -- the updated data (otherwise another writeIORef will overwrite
    -- our update)
    -- therefore here we choose to use "modifyIORef" in an atomic manner
    atomicModifyIORef' refCurrent ((,()) . (& dir .~ refNew))
    atomicModifyIORef' refNext ((,()) . (& revDir .~ refNew))

unshift, push :: Deque a -> a -> IO ()

unshift = dqInsert eNext ePrev
push    = dqInsert ePrev eNext

shift, pop :: Deque a -> IO (Maybe a)

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
