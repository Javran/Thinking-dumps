{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Functors where

import Control.Monad

newtype StateMT s m a = SM (s -> m (a,s)) deriving (Functor)

instance Monad m => Applicative (StateMT s m) where
    pure v = SM (\s -> pure (v,s))
    (<*>) = ap

bindStateMT :: Monad m => StateMT s m a -> (a -> StateMT s m b) -> StateMT s m b
bindStateMT x f = SM (\s ->
                      let SM x' = x
                      in x' s >>= \(v,s') ->
                      let SM f' = f v
                      in f' s')

instance Monad m => Monad (StateMT s m) where
    (>>=) = bindStateMT
