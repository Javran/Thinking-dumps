module Stream where

import qualified Control.Category as Cat
import Control.Arrow

data Stream a = Cons a (Stream a)

newtype StreamMap i o = SM { getSM :: Stream i -> Stream o }

arrSM :: (i -> o) -> StreamMap i o
arrSM f = SM ar
  where
    ar ~(Cons x xs) = Cons (f x) (ar xs)

composeSM :: StreamMap a b -> StreamMap b c -> StreamMap a c
composeSM (SM f) (SM g) = SM (g . f)

-- split a stream of pairs into two streams: one contains the first part,
-- and the other the second part
splitSM :: Stream (a,b) -> (Stream a, Stream b)
splitSM (Cons (a,b) xs) = (Cons a as, Cons b bs)
  where
    (as,bs) = splitSM xs

-- merge two streams into one
mergeSM :: Stream a -> Stream b -> Stream (a,b)
mergeSM (Cons a as) (Cons b bs) = Cons (a,b) (mergeSM as bs)

firstSM :: StreamMap a b -> StreamMap (a,d) (b,d)
firstSM (SM f) = SM ar
  where
    ar xs = mergeSM (f as) ds
      where
        (as,ds) = splitSM xs

instance Cat.Category StreamMap where
    id = arrSM id
    g . f = composeSM f g

instance Arrow StreamMap where
    arr = arrSM
    first = firstSM

constSM :: a -> Stream a
constSM x = let s = Cons x s in s

nats :: Integral i => Stream i
nats = Cons 1 (getSM (arrSM succ) nats)

streamToList :: Stream a -> [a]
streamToList ~(Cons a as) = a : streamToList as
