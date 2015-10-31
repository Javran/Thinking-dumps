{-# LANGUAGE KindSignatures #-}
module Deque
  ( mkDeque
  , push
  , pop
  , shift
  , unshift
  ) where

import Data.IORef

data Deque a

data Element (ref :: * -> *) a
  = Guard
      { ePrev :: ref (Element ref a)
      , eNext :: ref (Element ref a)
      }
  | Item
      { ePrev :: ref (Element ref a)
      , eNext :: ref (Element ref a)
      , eContent :: ref a
      }

mkDeque :: IO (Deque a)
mkDeque = undefined

push, unshift :: Deque a -> a -> IO (Deque a)

push = undefined
unshift = undefined

pop, shift :: Deque a -> IO (Maybe a)
pop = undefined
shift = undefined
