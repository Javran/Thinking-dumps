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

-- TODO: I don't know what's a proxy pattern
-- maybe I need to design some protocol and implement
-- all of the message handling?

-- INVARIANT: "Deque a" always point to a guard element
type Deque a = IORef (Element a)
type NodeRef a = IORef (Element a)

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

type RefLens a = Simple Lens (Element a) (NodeRef a)

mkDeque :: IO (Deque a)
mkDeque = mfix $ \r -> newIORef (Guard r r)

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref modify = atomicModifyIORef' ref ( (,()) . modify)

dqInsert :: RefLens a -> RefLens a -> NodeRef a -> a -> IO ()
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
    refNew <- newIORef newNode
    -- there are subtle issues regarding using "writeIORef":
    -- when refGuard and refNext is pointing to the same location,
    -- writeIORef will change its content and we have to retrieve
    -- the updated data (otherwise another writeIORef will overwrite
    -- our update)
    -- therefore here we choose to use "modifyIORef" in an atomic manner
    atomicModifyIORef_ refCurrent (& dir .~ refNew)
    atomicModifyIORef_ refNext (& revDir .~ refNew)

dqDelete :: RefLens a -> RefLens a -> NodeRef a -> IO (Maybe a)
dqDelete dir revDir refCurrent = do
    curNode <- readIORef refCurrent
    let refXNode = curNode ^. dir
    xNode <- readIORef refXNode
    let refNewNext = xNode ^. dir
    atomicModifyIORef_ refCurrent (& dir .~ refNewNext)
    atomicModifyIORef_ refNewNext (& revDir .~ refCurrent)
    return (xNode ^? eContent)

unshift, push :: Deque a -> a -> IO ()

unshift = dqInsert eNext ePrev
push    = dqInsert ePrev eNext

shift, pop :: Deque a -> IO (Maybe a)

shift dq = do
    -- for readability
    b <- nullDeque dq
    if b
      then return Nothing
           -- note that the meaning of "Maybe a" from dqDelete and that of return type "Maybe a"
           -- from this function are different, but
           -- since at this point deque is not empty, the result of dqDelete
           -- happens to be the correct return value of this function
           -- (same for "pop" function)
      else dqDelete eNext ePrev dq

pop dq = do
    b <- nullDeque dq
    if b
      then return Nothing
      else dqDelete ePrev eNext dq

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
