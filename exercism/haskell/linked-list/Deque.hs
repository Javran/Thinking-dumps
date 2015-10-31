module Deque
  ( mkDeque
  , push
  , pop
  , shift
  , unshift
  ) where

data Deque a

mkDeque :: IO (Deque a)
mkDeque = undefined

push, unshift :: Deque a -> a -> IO (Deque a)

push = undefined
unshift = undefined

pop, shift :: Deque a -> IO (Maybe a)
pop = undefined
shift = undefined
