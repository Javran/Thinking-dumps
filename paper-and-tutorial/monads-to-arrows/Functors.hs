{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TupleSections #-}
module Functors where

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad

newtype StateMT s m a = SM (s -> m (a,s)) deriving (Functor)

-- for "ap" to be usable the constrain "m" is necessary
-- although we can write the implementation, let just save some human work
-- and let machine do its job
instance Monad m => Applicative (StateMT s m) where
    pure v = SM (\s -> pure (v,s))
    (<*>) = ap

instance Monad m => Monad (StateMT s m) where
    (SM x) >>= f = SM
         (x >=> \(v,s') ->
          let SM f' = f v in f' s')

liftState :: Monad m => m a -> StateMT s m a
liftState m = SM $ \s -> m >>= \a -> pure (a,s)

fetch :: Monad m => StateMT s m s
fetch = SM $ \s -> pure (s,s)

-- returns the old state
store :: Monad m => s -> StateMT s m s
store s = SM $ \oldS -> pure (oldS,s)

store_ :: Monad m => s -> StateMT s m ()
store_ = void . store

newtype MaybeFunctor a b c = MF (a b (Maybe c))

-- the implementation can be guided just by type
liftMaybe :: Arrow a => a b c -> MaybeFunctor a b c
liftMaybe f = MF (f >>> arr Just)

instance (Arrow a, ArrowChoice a) => Cat.Category (MaybeFunctor a) where
    id = MF (arr Just)
    (MF g) . (MF f) = MF (f >>> arr dispatch >>> (g ||| arr id))
      where
        dispatch z = case z of
            Just c -> Left c
            Nothing -> Right Nothing

instance (Arrow a, ArrowChoice a) => Arrow (MaybeFunctor a) where
    arr f = MF (arr (Just . f))
    first (MF f) = MF (first f >>>
                       arr (\(c',d) -> (,d) <$> c'))
