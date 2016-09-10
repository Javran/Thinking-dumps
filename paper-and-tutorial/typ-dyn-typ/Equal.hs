{-# LANGUAGE RankNTypes, PolyKinds #-}
module Equal where

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
