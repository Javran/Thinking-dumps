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
data Z
data S n
data HTrue
data HFalse

class Includes e s where
    -- injection
    inj :: e w -> s w
    -- projection
    prj :: s w -> Maybe (e w)
