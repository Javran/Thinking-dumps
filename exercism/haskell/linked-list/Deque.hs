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

push refGuard v = do
    guardNode <- readIORef refGuard
    -- guard's next pointer points to the head
    let refHead = eNext guardNode
    headNode <- readIORef refHead
    let newNode = Item refGuard refHead v
    refNew <- newIORef newNode
    let newGuardNode = guardNode { eNext = refNew }
        -- now it's the node right after (new) head node
        newHeadNode = headNode { ePrev = refNew }
    writeIORef refGuard newGuardNode
    writeIORef refHead newHeadNode
    return refGuard

unshift = undefined

pop, shift :: Deque a -> IO (Maybe a)
pop = undefined
shift = undefined
