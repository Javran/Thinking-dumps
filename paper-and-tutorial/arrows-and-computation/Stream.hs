module Stream where

import qualified Control.Category as Cat
import Control.Arrow
import Data.Function

data Stream a = Cons a (Stream a)

newtype StreamMap i o = SM { getSM :: Stream i -> Stream o }

instance Functor Stream where
    fmap f = fix $ \f' (~(Cons x xs)) -> Cons (f x) (f' xs)

instance Functor (StreamMap i) where
    fmap g (SM f) = SM $ fmap g . f

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

zipSMBy :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipSMBy f (Cons a as) (Cons b bs) = Cons (f a b) (zipSMBy f as bs)

-- merge two streams into one
zipSM :: Stream a -> Stream b -> Stream (a,b)
zipSM = zipSMBy (,)

firstSM :: StreamMap a b -> StreamMap (a,d) (b,d)
firstSM (SM f) = SM ar
  where
    ar xs = zipSM (f as) ds
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

tailSM :: Stream a -> Stream a
tailSM (Cons _ xs) = xs

fibs :: Integral i => Stream i
fibs = Cons 0 (Cons 1 (zipSMBy (+) fibs (tailSM fibs)))

-- split an "Either" stream into its "Left" parts and "Right" parts
eitherSplitSM :: Stream (Either a b) -> (Stream a, Stream b)
eitherSplitSM = fix $ \f (Cons ab xs) ->
    let (sa,sb) = f xs in
    case ab of
        Left a -> (Cons a sa, sb)
        Right b -> (sa, Cons b sb)

instance ArrowChoice StreamMap where
    left (SM f) = SM $ \ (~(Cons bd xs)) -> case bd of
        Left _ ->
            let bs = leftOnly (Cons bd xs)
                Cons c _ = f bs
            in Cons (Left c) (f' xs)
        Right d -> Cons (Right d) (f' xs)
      where
        (SM f') = left (SM f)
        leftOnly (Cons x xs) = case x of
            Left v -> Cons v (leftOnly xs)
            Right _ -> leftOnly xs
