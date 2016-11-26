module Stream where

import qualified Control.Category as Cat
import Control.Arrow

data Stream a = Cons a (Stream a)

newtype StreamMap i o = SM (Stream i -> Stream o)

arrSM :: (i -> o) -> StreamMap i o
arrSM f = SM ar
  where
    ar ~(Cons x xs) = Cons (f x) (ar xs)

composeSM :: StreamMap a b -> StreamMap b c -> StreamMap a c
composeSM (SM f) (SM g) = SM (g . f)

firstSM :: StreamMap a b -> StreamMap (a,d) (b,d)
firstSM (SM f) = SM $ \ ~(Cons (a,d) s) -> Cons (_,d) _
