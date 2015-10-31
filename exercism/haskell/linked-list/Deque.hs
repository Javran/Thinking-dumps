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

push = undefined
unshift = undefined

pop, shift :: Deque a -> IO (Maybe a)
pop = undefined
shift = undefined
