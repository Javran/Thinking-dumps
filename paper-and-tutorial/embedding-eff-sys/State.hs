{-# LANGUAGE
    KindSignatures
  , UndecidableInstances
  , ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DataKinds
  , TypeOperators
  , TypeFamilies
  , RebindableSyntax
  #-}
module State where

import Prelude
import GHC.Exts
import TypeLevelSets hiding (Nub, nub, Unionable, Nubable, union {-, IsSet -})
import EffSys hiding (Effect, Eff, R, Update, update)
import qualified EffSys (Effect)

-- type IsSet s = ((s ~ Nub (Sort s)) :: Constraint)

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

class Nubable t v where
    nub :: Set t -> Set v

instance Nubable '[] '[] where
    nub Empty = Empty

instance Nubable '[e] '[e] where
    nub (Ext e Empty) = (Ext e Empty)

instance Nubable ((k :-> b :! s) ': as) as' =>
    Nubable ((k :-> a :! s) ': (k :-> b :! s) ': as) as' where
    nub (Ext _ (Ext x xs)) = nub (Ext x xs)

instance Nubable ((k :-> a :! RW) ': as) as' =>
    Nubable ((k :-> a :! s) ': (k :-> a :! t) ': as) as' where
    nub (Ext _ (Ext (k :-> (a :! _)) xs)) = nub (Ext (k :-> (a :! (Eff::(Effect RW)))) xs)

instance Nubable ((j :-> b :! t) ': as) as' =>
    Nubable ((k :-> a :! s) ': (j :-> b :! t) ': as) ((k :-> a :! s) ': as') where
    nub (Ext (k :-> (a :! s)) (Ext (j :-> (b :! t)) xs)) =
        Ext (k :-> (a :! s)) (nub (Ext (j :-> (b :! t)) xs))

type UnionS s t = Nub (Sort (Append s t))
type Unionable s t =
    ( Sortable (Append s t)
    , Nubable (Sort (Append s t)) (Nub (Sort (Append s t)))
    , Split s t (Union s t))

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
    -- update _ = error "impossible"
{-
  if [u :-> b :! s, ...] ~~> as'
  then [v :-> a :! W, u :-> b :! s, ...] ~~> as'
-}
instance Update ((u :-> b :! s) ': as) as' =>
  Update ((v :-> a :! 'W) ': (u :-> b :! s) ': as) as' where
    update (Ext _ (Ext e xs)) = update (Ext e xs)
    -- update _ = error "impossible"

{-
  if [u :-> b :! s, ...] ~~> as'
  then [v :-> a :! R, u :-> b :! s, ...] ~~> (v :-> a :! R) : as'
-}
instance Update ((u :-> b :! s) ': as) as' =>
  Update ((v :-> a :! 'R) ': (u :-> b :! s) ': as)
         ((v :-> a :! 'R) ': as') where
    update (Ext e (Ext e' xs)) = Ext e (update (Ext e' xs))
    -- update _ = error "impossible"

-- a bit more complicated than what's written in the paper
type IntersectR s t = (Sortable (Append s t), Update (Sort (Append s t)) t)

intersectR :: (Writes s ~ s, Reads t ~ t, IsSet s, IsSet t, IntersectR s t) =>
              Set s -> Set t -> Set t
intersectR s t = update (bsort (append s t))

-- "s" is a list of things that the computation can read and write
-- and "Reads s" and "Writes s" break the list into two
data State s a = State
  { runState :: Set (Reads s) -> (a, Set (Writes s)) }


union :: (Unionable s t) => Set s -> Set t -> Set (UnionS s t)
union s t = nub (bsort (append s t))

-- I think I've done everything I can think of, no clue about
-- the error I'm getting, so I'll skip rest of this part for now.
-- TODO: not working as expected,
-- we might have to change everything related to "Nub" a bit?
-- the re-definition of "Nub" might not be the problem.
instance EffSys.Effect State where
    type Inv State s t = ( IsSet s, IsSet (Reads s), IsSet (Writes s)
                         , IsSet t, IsSet (Reads t), IsSet (Writes t)
                         , Reads (Reads t) ~ Reads t
                         , Writes (Writes s) ~ Writes s
                         , Split (Reads s) (Reads t) (Reads (UnionS s t))
                         , Unionable (Writes s) (Writes t)
                         , IntersectR (Writes s) (Reads t)
                         , Writes (UnionS s t) ~ UnionS (Writes s) (Writes t))

    type Unit State = '[]
    type Plus State s t = UnionS s t
    pure x = State (\Empty -> (x, Empty))
    (State e) >>= k = State $ \st ->
        let (sR,tR) = split st
            (a,sW) = e sR
            (b,tW) = (runState $ k a) (sW `intersectR` tR)
        in (b,sW `union` tW)
