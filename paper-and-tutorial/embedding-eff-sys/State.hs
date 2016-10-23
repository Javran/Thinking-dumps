{-# LANGUAGE
    KindSignatures
  , ConstraintKinds
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
-- note that ":!" binds tighter than ":->"
-- so "v :-> a :! f" means "v :-> (a :! f)"
-- reads "variable v has type a and effect action f"

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

{-
  this State monad is more flexible than the State monad we usually see
  because it allows storing value of different types under the same "var" label
  but this is at the cost of code complexity:
  personally I think the implementation of Update is messy and
  it's hard to just look at the code and tell what's going on
-}

{-
  let's denote Update like an arrow: "~~>"
  TODO: just translation, I'm still not sure what this means
  if [v :-> a :! R, ...] ~~> as'
  then [v :-> a :! W, v :-> b :! R, ...] ~~> as'
-}
instance Update ((v :-> a :! 'R) ': as) as' =>
  Update (  (v :-> a :! 'W)
         ': (v :-> b :! 'R)
         ': as) as' where
    -- for all wildcards, we've already know what it must be
    -- so there's no point to check them again
    update (Ext (v :-> (a :! _)) (Ext _ xs)) =
        update (Ext (v :-> (a :! (Eff :: Effect 'R))) xs)
    update _ = error "impossible"
{-
  if [u :-> b :! s, ...] ~~> as'
  then [v :-> a :! W, u :-> b :! s, ...] ~~> as'
-}
instance Update ((u :-> b :! s) ': as) as' =>
  Update ((v :-> a :! 'W) ': (u :-> b :! s) ': as) as' where
    update (Ext _ (Ext e xs)) = update (Ext e xs)
    update _ = error "impossible"

{-
  if [u :-> b :! s, ...] ~~> as'
  then [v :-> a :! R, u :-> b :! s, ...] ~~> (v :-> a :! R) : as'
-}
instance Update ((u :-> b :! s) ': as) as' =>
  Update ((v :-> a :! 'R) ': (u :-> b :! s) ': as)
         ((v :-> a :! 'R) ': as') where
    update (Ext e (Ext e' xs)) = Ext e (update (Ext e' xs))
    update _ = error "impossible"
