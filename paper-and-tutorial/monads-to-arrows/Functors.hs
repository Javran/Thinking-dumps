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
