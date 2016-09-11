{-# LANGUAGE RankNTypes, PolyKinds, ExistentialQuantification #-}
module Equal where

import Data.Functor.Identity

{-
   Leibnitz's law, saying if a, b are identical,
   then they should have identical properties as well.
   the original claim should be "forall f. f a <-> f b",
   but it's proven to be equivalent to "forall f. f a -> f b"

   notes:
   * not sure how this works exactly for now, but without bottoms
     id :: forall a. a -> a seems to be the only type that fits
   * the only thing we know is that "f" is a valid type constructor
     and nothing more. without knowing the structure of "f", we cannot
     construct something funny to fit the type
-}
newtype Equal (a :: k) (b :: k) = Equal (forall f. f a -> f b)

-- polymorphic kind allows us to construct proofs of various kinds
eqInt :: Equal Int Int
eqInt = Equal id

eqList :: Equal [] []
eqList = Equal id

eqFunc :: Equal (->) (->)
eqFunc = Equal id

-- "reflex" generalizes everything above.
reflex :: Equal a a
reflex = Equal id

trans :: Equal a b -> Equal b c -> Equal a c
trans ab bc = case (ab,bc) of
  (Equal f, Equal g) -> Equal (g . f)

{-
   motivation: when given "Equal a b", for a type "t" we should be able to substitite
   every occurrence of "a" in it by "b".
   - now let "c a" be the input type "t"
     (here we call it "ta" to distinguish from "tb", the resulting type)
     here we are using some "newtype" definitions to rewrite "t" so that
     it end up being a "function application" on type level.
   - notice that Equal a b provides us with a function: "forall f. f a -> f b",
     so with "c a", we should get "c b" by using this function.
   - for the "c b -> tb" part, we are just unwrapping whatever "newtype" we have
     just defined to end up with the intended resulting type.
-}
subst :: (ta -> c a) -> (c b -> tb) -> Equal a b -> ta -> tb
subst from to (Equal ab) = to . ab . from


{- example about how to convert from (a,a) to (b,b) knowing Equal a b,
   basically we will need a lambda abstraction on type level to make the type
   a "type function application" by type "a", then we'll have the chance to replace it
   by "b"
-}
newtype Pair x = Pair { unPair :: (x,x) }

substPair :: Equal a b -> (a,a) -> (b,b)
substPair = subst Pair unPair

-- "FlipEqual y _" is just "Equal _ y" and the hole is where
-- we are going to play with
newtype FlipEqual y x = Flip { unFlip :: Equal x y }

-- notice that "symm" shows us why passing a function of type "forall f. f a -> f b"
-- is enough for a type equality proof: now we can construct "forall f. f b -> f a"
-- out from it!
symm :: Equal a b -> Equal b a
symm ab = subst Flip unFlip ab reflex

-- "g (f _)" is all what we need,
newtype Comp g f x = Comp { unComp :: g (f x) }

arg :: Equal a b -> Equal (f a) (f b)
arg ab = Equal (subst Comp unComp ab)

-- apply a substitution to the argument of a type constructor
rewrite :: Equal a b -> Equal c (f a) -> Equal c (f b)
rewrite eqAB eqCFa = trans eqCFa eqFaFb
  where
    eqFaFb = arg eqAB

-- we need "h (_ a)" in order to replace the hole with something else
newtype Haf h a f = Haf { unHaf :: h (f a) }

-- like "arg" but acts on the function part
func :: Equal f g -> Equal (f a) (g a)
func eqFG = Equal (subst Haf unHaf eqFG)

rewrite' :: Equal a b -> Equal c (f a d) -> Equal c (f b d)
rewrite' eqAB eqCFad = trans eqCFad (func (arg eqAB))

-- TODO: now that Haskell does has Kind polymorphism, but I'm not sure
-- of this part. skipping it for now.

congruence :: Equal a b -> Equal c d -> Equal (f a c) (f b d)
congruence eqAB eqCD = rewrite eqCD (rewrite' eqAB reflex)

-- now that "~" is taken, so we have to use "~~" instead
class TypRep tpr where
    (~~) :: tpr a -> tpr b -> Maybe (Equal a b)

-- using "Identity" so that we could have "forall f. f a -> f b" become
-- "Identity a -> Identity b", and which is exactly just the "a -> b" function
-- we are looking for, again most stuff about "Equal _ _" are just identity functions
-- on value level.
{-# ANN coerce "HLint: ignore Redundant bracket" #-}
{-# ANN coerce "HLint: ignore Eta reduce" #-}
coerce :: Equal a b -> (a -> b)
coerce eqAB = subst Identity runIdentity eqAB

data Dynamic typeRep = forall a. a ::: typeRep a
