{-# LANGUAGE
    GADTs
  , TypeFamilies
  #-}
module Wrapped where

import qualified Prelude as P
import EffSys (Effect(..))

data Monad m t a where
    Wrap :: P.Monad m => m a -> Monad m () a

unWrap :: Monad m t a -> m a
unWrap (Wrap m) = m

-- to prove that the standard monad fits into this framework
-- with a trivial monoid
instance (P.Monad m) => Effect (Monad m) where
    type Unit (Monad m) = ()
    type Plus (Monad m) s t = ()
    pure x = Wrap (P.return x)
    Wrap x >>= f = Wrap ((P.>>=) x (unWrap P.. f))
