{-# LANGUAGE
    KindSignatures
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DataKinds
  , TypeOperators
  , TypeFamilies
  #-}
module State where

import TypeLevelSets hiding (Nub)
import EffSys hiding (Effect, Eff, R, Update, update)

data Eff = R | W | RW

data Effect (s :: Eff) = Eff

data (:!) (a :: *) (s :: Eff) = a :! (Effect s)
infixl 3 :!

type family Reads t where
  Reads '[] = '[]
  Reads ((v :-> a :! 'R) ': s) = (v :-> a :! 'R) ': (Reads s)
  Reads ((v :-> a :! 'RW) ': s) = (v :-> a :! 'R) ': (Reads s)
  Reads ((v :-> a :! 'W) ': s) = Reads s

type family Writes t where
  Writes '[] = '[]
  Writes ((v :-> a :! 'W) ': s) = (v :-> a :! 'W) ': (Writes s)
  Writes ((v :-> a :! 'RW) ': s) = (v :-> a :! 'W) ': (Writes s)
  Writes ((v :-> a :! 'R) ': s) = Writes s

type family Nub t where
  Nub '[] = '[]
  Nub '[e] = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub ((v :-> a :! f) ': (v :-> a :! g) ': s) =
      -- it is usually not making sense that you have two abstract types
      -- like "f" and "g" and you end up with a concrete one "RW"
      -- but think: if "f" and "g" are the same, it would have been
      -- captures by the previous pattern matching,
      -- so here it is safe to assume that "f" and "g" are different
      -- with this idea in mind, it's not hard to see the result of "f" + "g"
      -- must be RW
      Nub ((v :-> a :! 'RW) ': s)
  Nub (e ': f ': s) = e ': (Nub (f ': s))

type UnionS s t = Nub (Sort (Append s t))

class Update s t where
    update :: Set s -> Set t

instance Update xs '[] where
    update _ = Empty

instance Update '[e] '[e] where
    update s = s

instance Update ((v :-> a :! R) ': as) as' => Update ((v :-> a :! W) ': (v :-> b :! R) ': as) as' where
    update (Ext (v :-> (a :! _)) (Ext _ xs)) = update (Ext (v :-> (a :! (Eff::(Effect R)))) xs)
