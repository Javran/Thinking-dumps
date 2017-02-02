{-# LANGUAGE
    GADTs
  , FlexibleInstances
  , MultiParamTypeClasses
  , KindSignatures
  , ScopedTypeVariables
  , TypeOperators
  #-}
module OpenUnion1
  ( Union
  , inj, prj
  , decomp, weaken
  , (:>)
  , Member
  ) where

-- http://okmij.org/ftp/Haskell/extensible/OpenUnion1.hs

import Data.Typeable

-- TODO: why this is necessary?
newtype Id x = Id x

data Union r v where
    Union :: (Functor t, Typeable t) => Id (t v) -> Union r v

instance Functor (Union r) where
    fmap f (Union (Id v)) = Union (Id (fmap f v))

-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

-- means "t" is a member of "r" ("r" is phantom?)
class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

inj :: (Functor t, Typeable t, Member t r) => t v -> Union r v
inj x = Union (Id x)

-- is "gcast1" casting across type of kind * -> * ?
-- by using typed hole, the value "v" below is of type (Id (_ v)), where
-- the "_" part is still a myth to me .. but somehow it is possible for that to match
-- "Id (t v)" thus cast-able
prj :: (Functor t, Typeable t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id (x :: t v)) <- gcast1 v = Just x
prj _ = Nothing

-- TODO: inj <=> weaken, prj <=> decomp?
weaken :: (Typeable t, Functor t) => Union r w -> Union (t :> r) w
weaken (Union x) = Union x

decomp :: Typeable t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)
