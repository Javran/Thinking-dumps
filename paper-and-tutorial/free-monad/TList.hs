{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module TList where

-- http://okmij.org/ftp/Haskell/extensible/TList.hs

import Data.Void

infixr 1 :>

data (a :> b) w = H (a w) | T (b w)

-- Functor sum.
instance (Functor s, Functor t) => Functor (s :> t) where
    fmap f (H x) = H (fmap f x)
    fmap f (T x) = T (fmap f x)

-- probably we can use promoted types here..
{-

TODO: could we use GHC.TypeLits for this?

- https://hackage.haskell.org/package/base-4.9.1.0/docs/GHC-TypeLits.html
- http://ponies.io/posts/2014-07-30-typelits.html

I think this can eliminate the need for TEQ

-}

data Z
data S n
data HTrue
data HFalse

class Includes e s where
    -- injection
    inj :: e w -> s w
    -- projection
    prj :: s w -> Maybe (e w)

