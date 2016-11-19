{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Functors where

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
