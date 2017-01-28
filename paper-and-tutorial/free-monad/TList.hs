{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , KindSignatures
  , TypeFamilies
  , FlexibleInstances
  , DataKinds
  #-}
module TList where

import Data.Void
import Data.Proxy

{-# ANN module "HLint: ignore Eta reduce" #-}

-- http://okmij.org/ftp/Haskell/extensible/TList.hs

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

I think this can eliminate the need for TEQ: we can just use "CmpNat n1 n2",
if we get 'EQ as the result, n1 and n2 are the same, otherwise they are not.

-}

data Nat = Z | S Nat

{-
to be TCode seems to be a way of converting a type into a natural number
so every type (that TCode can encode) can be distinguished from other types
-}
-- TCode :: * -> Nat
type family TCode (n :: * -> *) :: Nat

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
class Includes' (b :: Bool) e e1 s where
    inj' :: Proxy b -> e w -> (e1 :> s) w
    prj' :: Proxy b -> (e1 :> s) w -> Maybe (e w)

-- when e is the same type as e1
instance (e ~ e1) => Includes' 'True e e1 t where
    -- injection, always safe.
    inj' _ e = H e
    -- projection, if the test "e ~ e1" passes,
    -- we will have just enough info to safely cast it
    -- yep, sounds like Typeable
    prj' _ (H e) = Just e
    prj' _ (T _) = Nothing

-- when e is not the same type as e1, let's choose the other branch
instance Includes e t => Includes' 'False e e1 t where
    inj' _ e = T (inj e)
    prj' _ (H _) = Nothing
    prj' _ (T e) = prj e

type family TEQ (n1 :: Nat) (n2 :: Nat) :: Bool
type instance TEQ 'Z 'Z           = 'True
type instance TEQ ('S _) 'Z       = 'False
type instance TEQ 'Z ('S _)       = 'False
type instance TEQ ('S n1) ('S n2) = TEQ n1 n2

{-

try using OrdToEq this way:
- (CmpNat n1 n2 ~ ord, OrdToEq ord ~ True) => _
- (CmpNat n1 n2 ~ ord, OrdToEq ord ~ False) => _

-}
type family OrdToEq (o :: Ordering) :: Bool where
    OrdToEq 'EQ = 'True
    OrdToEq 'LT = 'False
    OrdToEq 'GT = 'False
