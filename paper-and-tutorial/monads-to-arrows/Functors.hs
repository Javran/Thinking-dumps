{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Functors where

import Control.Monad

newtype StateMT s m a = SM (s -> m (a,s)) deriving (Functor)

instance Monad m => Applicative (StateMT s m) where
    pure v = SM (\s -> pure (v,s))
    SM f <*> SM v = SM (\s -> f s >>= \(f',s') -> v s' >>= \(v',s'') -> pure (f' v',s''))

instance Monad m => Monad (StateMT s m) where
    x >>= f = SM (\s ->
                  let SM x' = x
                  in x' s >>= \(v,s') ->
                  let SM f' = f v
                  in f' s')
