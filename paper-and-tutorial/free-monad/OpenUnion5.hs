{-# LANGUAGE
    KindSignatures, ScopedTypeVariables
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , PolyKinds
  , DataKinds
  , GADTs
  , TypeOperators
  , TypeFamilies
  , UndecidableInstances
  #-}
module OpenUnion5 where

-- http://okmij.org/ftp/Haskell/extensible/OpenUnion5.hs

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits
import Data.Proxy

-- "r" is thought as an universe and "Int" value being the index of "t"
-- (not sure?) basically we can hide "t" behind the universe and use "Union r" in place of it
data Union (r :: [* -> *]) v where
    Union :: !Int -> t v -> Union r v

inj' :: Int -> t v -> Union r v
inj' = Union

-- type equivalence is established by instance finding through
-- type family FindElem, as long as this "Int" is filled in as the result of
-- instance finding, it *is* the proof of type equivalence.
prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x)
    | n == n' = Just (unsafeCoerce x)
    | otherwise = Nothing

-- closed type family, find "t" within the universe "r",
-- the program should not typecheck if it turns out "t" is not in the universe.
type family FindElem (t :: * -> *) r :: Nat where
    FindElem t (t ': r) = 0
    FindElem t (any ': r) = 1 + FindElem t r

-- type equivalence to promoted Bool
type family EQU (a :: k) (b :: k) :: Bool where
    EQU a a = 'True
    EQU a b = 'False

-- if we can find "t" in universe "r", then we can establish that "t" is a member of "r"
class (KnownNat (FindElem t r)) => Member (t :: * -> *) r where
    inj :: t v -> Union r v
    prj :: Union r v -> Maybe (t v)

-- the type level proof is automatically found and reflected on value level
instance (KnownNat n, FindElem t r ~ n) => Member t r where
    inj = inj' (fromInteger (natVal (Proxy :: Proxy n)))
    prj = prj' (fromInteger (natVal (Proxy :: Proxy n)))
