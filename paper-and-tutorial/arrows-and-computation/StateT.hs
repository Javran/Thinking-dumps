module StateT where

import qualified Control.Category as Cat
import Control.Arrow

newtype StateT s arr i o = ST (arr (s,i) (s,o))

arrStateT :: Arrow arr => (i -> o) -> StateT s arr i o
arrStateT f = ST $ arr (\(~(s,i)) -> (s,f i))

compStateT :: Arrow arr => StateT s arr a b -> StateT s arr b c -> StateT s arr a c
compStateT (ST f) (ST g) = ST (f >>> g)

instance Arrow arr => Cat.Category (StateT s arr) where
    id = arrStateT id
    g . f = compStateT f g
