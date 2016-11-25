module Writer where

import qualified Control.Category as Cat
import Control.Arrow

newtype Writer m i o = W (i -> (m,o))

arrWriter :: Monoid m => (i -> o) -> Writer m i o
arrWriter f = W $ \a -> (mempty, f a)

composeWriter :: Monoid m => Writer m a b -> Writer m b c -> Writer m a c
composeWriter (W f) (W g) = W $ \a ->
    let (w1,b) = f a
        (w2,c) = g b
    in (mappend w1 w2, c)

instance Monoid m => Cat.Category (Writer m) where
    id = arrWriter id
    g . f = composeWriter f g

instance Monoid m => Arrow (Writer m) where
    arr = arrWriter
    first (W f) = W $ \(b,d) ->
        let (m,c) = f b
        in (m, (c,d))
