{-# LANGUAGE TupleSections #-}
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

{-
Let's check laws that "left" has to satisfy:

- extension: left (arr f) === arr (f +++ id)

so applying a pure function "f" on "Left" values is the same as applying a function
on the input value, where "Left" values are applied with "f" and "Right" are kept intact.

- functor: left (f >>> g) === left f >>> left g

this is obvious, composition can be done either way, and it doesn't change the result.

- exchange: left f >>> arr (id +++ g) === arr (id +++ g) >>> left f

so if "g" is a pure function that acts only on "Right" part, then
it doesn't matter when "id +++ g" is applied before or after arrow "f" on "Left"

- unit: arr Left >>> left f === f >>> arr Left

apply then wrap = wrap then apply

- association: left (left f) >>> arr assocsum === pure assocsum >>> left f

where

assocsum (Left (Left a)) = Left a
assocsum (Left (Right b)) = Right (Left b)
assocsum (Right c) = Right (Right c)

(basically "assocsum" just re-associates Left / Right elements around)

TODO: so all laws are just talking about the relationship between "Left" part and the
whole thing, so we can we say that how "Left" or "Right" inputs interleave with each other
does not matter?

-}
instance ArrowChoice StreamMap where
    -- TODO: there are many possible implementations
    -- of "left" because while there are infinite ways of interleaving results.
    -- to figure out which one is the correct one, we might have to take
    -- a look at laws of ArrowChoice.
    -- in this case type doesn't really capture the notion of correct stream ordering
    -- so despite the following implementation of mine typechecks,
    -- I cannot say for sure this is the correct impl.
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

instance ArrowLoop StreamMap where
    -- TODO: typechecks, but I have no idea how this could work...
    loop (SM f) = SM $ \ sb ->
        let sbd = zipSM sb sd
            scd = f sbd
            sc = fst <$> scd
            sd = snd <$> scd
        in sc
