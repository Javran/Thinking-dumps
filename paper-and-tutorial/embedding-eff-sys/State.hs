{-# LANGUAGE
    KindSignatures
  , DataKinds
  , TypeOperators
  , TypeFamilies
  #-}
module State where

import TypeLevelSets
import EffSys hiding (Effect, Eff, R)

data Eff = R | W | RW

data Effect (s :: Eff) = Eff

data (:!) (a :: *) (s :: Eff) = a :! (Effect s)

type family Reads t where
  Reads '[] = '[]
  Reads ((v :-> a :! 'R) ': s) = (v :-> a :! 'R) ': (Reads s)
  Reads ((v :-> a :! 'RW) ': s) = (v :-> a :! 'R) ': (Reads s)
  Reads ((v :-> a :! 'W) ': s) = Reads s
