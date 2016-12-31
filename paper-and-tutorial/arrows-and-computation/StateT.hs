module StateT where

import qualified Control.Category as Cat
import Control.Arrow

{-

turns out implementing StateT without arrow notation is exactly
what we are asked to do for exercise 18. so we have done it!

-}

newtype StateT s arr i o = ST (arr (s,i) (s,o))

arrStateT :: Arrow arr => (i -> o) -> StateT s arr i o
arrStateT f = ST $ arr (\(~(s,i)) -> (s,f i))

compStateT :: Arrow arr => StateT s arr a b -> StateT s arr b c -> StateT s arr a c
compStateT (ST f) (ST g) = ST (f >>> g)

instance Arrow arr => Cat.Category (StateT s arr) where
    id = arrStateT id
    g . f = compStateT f g

firstStateT :: Arrow arr => StateT s arr a b -> StateT s arr (a,d) (b,d)
firstStateT (ST f) = ST (arr assoc >>> first f >>> arr unassoc)
  where
    assoc (s,(a,d)) = ((s,a),d)
    unassoc ((s,a),d) = (s,(a,d))

instance Arrow arr => Arrow (StateT s arr) where
    arr = arrStateT
    first = firstStateT
