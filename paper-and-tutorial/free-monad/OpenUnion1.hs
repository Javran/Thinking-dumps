{-# LANGUAGE
    GADTs
  , FlexibleInstances
  , MultiParamTypeClasses
  , KindSignatures
  , ScopedTypeVariables
  , TypeOperators
  #-}
module OpenUnion1 where

-- http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs

import Data.Typeable

-- TODO: why this is necessary?
newtype Id x = Id x

data Union (r :: * -> *) v where
    Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

instance Functor (Union r) where
    fmap f (Union (Id v)) = Union (Id (fmap f v))

-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)
