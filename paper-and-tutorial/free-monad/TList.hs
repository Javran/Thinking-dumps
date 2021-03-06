{-# LANGUAGE
    TypeOperators
  , UndecidableInstances
  , ScopedTypeVariables
  , FlexibleContexts
  , MultiParamTypeClasses
  , KindSignatures
  , TypeFamilies
  , FlexibleInstances
  , DataKinds
  #-}
module TList where

import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Reader (Reader)
import GHC.TypeLits

{-# ANN module "HLint: ignore Eta reduce" #-}

-- http://okmij.org/ftp/Haskell/extensible/TList.hs

-- note that this is just normal Data.Void because this one
-- has a type parameter so its kind becomes * -> *, while
-- Data.Void has kind of *
-- and this is not Data.Proxy either, as Proxy is inhabited.
data Void w

infixr 1 :>

data (a :> b) w = H (a w) | T (b w)

-- Functor sum.
instance (Functor s, Functor t) => Functor (s :> t) where
    fmap f (H x) = H (fmap f x)
    fmap f (T x) = T (fmap f x)

-- probably we can use promoted types here..
{-

- https://hackage.haskell.org/package/base-4.9.1.0/docs/GHC-TypeLits.html
- http://ponies.io/posts/2014-07-30-typelits.html

I think this can eliminate the need for TEQ: we can just use "CmpNat n1 n2",
if we get 'EQ as the result, n1 and n2 are the same, otherwise they are not.

-}

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
it's *incorrect* to have:

> OrdToEq (CmpNat (TCode e) (TCode e1)) ~ 'True

as oppose to the following one.

here we need "b" which works like a selector that guides instance finding
system to the correct branch.

-}
instance ( OrdToEq (CmpNat (TCode e) (TCode e1)) ~ b
         , Includes' b e e1 t) =>
   Includes e (e1 :> t)
 where
   inj = inj' (Proxy :: Proxy b)
   prj = prj' (Proxy :: Proxy b)

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

{-
-- we don't need this any more because of OrdToEq and CmpNat from GHC.TypeLits
type family TEQ (n1 :: Nat) (n2 :: Nat) :: Bool
type instance TEQ 'Z 'Z           = 'True
type instance TEQ ('S _) 'Z       = 'False
type instance TEQ 'Z ('S _)       = 'False
type instance TEQ ('S n1) ('S n2) = TEQ n1 n2
-}

{-

try using OrdToEq this way:
- (CmpNat n1 n2 ~ ord, OrdToEq ord ~ True) => _
- (CmpNat n1 n2 ~ ord, OrdToEq ord ~ False) => _

-}
type family OrdToEq (o :: Ordering) :: Bool where
    OrdToEq 'EQ = 'True
    OrdToEq 'LT = 'False
    OrdToEq 'GT = 'False

-- examples.
-- make sure the rightmost one is "Void"
type TestA = State Char :> Reader Int :> Identity :> Void
type TestB = State Char :> Identity :> Reader Int :> Void
type TestC = State Char :> State Int :> Reader Int :> Void

type instance TCode Identity = 0
type instance TCode (Reader Int) = 1
type instance TCode (State Char) = 2
type instance TCode (State Int) = 3

test1 :: Includes Identity t => t Int
test1 = inj (Identity 10)

{-
this would not typecheck if the rightmost one is not "Void".
GHC would complain "No instance for (Includes Identity Identity)"
so it seems that for the long chain of ":>", instance finding won't find the rightmost one..
-}
test2 :: TestA Int
test2 = test1

-- but the following one does typecheck
test3 :: TestB Int
test3 = test1

{-
-- this will never typecheck because it simply doesn't have "Identity" in set.
test4 :: TestC Int
test4 = test1
-}
