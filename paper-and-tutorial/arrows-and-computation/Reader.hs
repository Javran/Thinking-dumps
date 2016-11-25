module Reader where

import qualified Control.Category as Cat
import Control.Arrow

newtype Reader s i o = R ((s,i) -> o)

arrReader :: (i -> o) -> Reader s i o
arrReader f = R (f . snd)

composeReader :: Reader s a b -> Reader s b c -> Reader s a c
composeReader (R f) (R g) = R $ \ (~(s,a)) ->
    let b = f (s,a)
        c = g (s,b)
    in  c

instance Cat.Category (Reader s) where
    id = arrReader id
    g . f = composeReader f g

instance Arrow (Reader s) where
    arr = arrReader
    first (R f) = R $ \ (~(s,(b,d))) -> (f (s,b),d)
