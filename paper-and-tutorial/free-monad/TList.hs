{-# LANGUAGE TypeOperators, MultiParamTypeClasses, KindSignatures, TypeFamilies #-}
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

{-
to be TCode seems to be a way of converting a type into a natural number
so it can be distinguished from other types
-}
-- TCode :: * -> Nat
type family TCode (n :: * -> *) :: *

{-
a relation that "e" is one member of "s".
an injection raises "e" to "s" and is always safe,
while a projection tries to extract "e" from "s"
and can potentially result in failure.
-}
class Includes e s where
    -- injection
    inj :: e w -> s w
    -- projection
    prj :: s w -> Maybe (e w)

{-
"b" is a controller for allowing type level boolean to be computed
and used on making value-level decisions (by instances)
-}
class Includes' b e e1 s where
    inj' :: b -> e w -> (e1 :> s) w
    prj' :: b -> (e1 :> s) w -> Maybe (e w)

-- TEQ :: Nat -> Nat -> Bool
type family TEQ n1 n2 :: *
type instance TEQ Z Z           = HTrue
type instance TEQ (S n) Z       = HFalse
type instance TEQ Z (S n)       = HFalse
type instance TEQ (S n1) (S n2) = TEQ n1 n2
