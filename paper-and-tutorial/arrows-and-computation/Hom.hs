module Hom where

import qualified Control.Category as Cat
import Control.Arrow

-- homogeneous functions

{-
  "BalTree" is a collection of 2^n elements:

  - "Zero _" contains 1 = 2^0 element
  - "Succ (Zero (Pair _ _))" contains 2 = 2^1 elements

  note that the constructors form a string, which encodes the depth
  of this tree.
-}

data BalTree a = Zero a | Succ (BalTree (Pair a))

type Pair a = (a,a)

{-
  seems we can expand it indefinitely:

  Hom a b
  <=> (a -> b) :&: Hom (Pair a) (Pair b)
  <=> (a -> b) :&: (Pair a -> Pair b) :&: Hom (Pair (Pair a)) (Pair (Pair b))
  ...

-}
data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

-- I have to say this is a smart way of keeping track
apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: fs) t = case t of
    Zero x -> Zero (f x)
    Succ t' -> Succ (apply fs t')

arrHom :: (a -> b) -> Hom a b
arrHom f = f :&: arrHom (f *** f)

composeHom :: Hom a b -> Hom b c -> Hom a c
composeHom (a :&: as) (b :&: bs) = (b . a) :&: composeHom as bs

instance Cat.Category Hom where
    id = arrHom id
    g . f = composeHom f g

firstHom :: Hom a b -> Hom (a,d) (b,d)
firstHom (f :&: fs) = first f :&: (tr >>> firstHom fs >>> tr)
  where
    transpose :: ((a,b),(c,d)) -> ((a,c),(b,d))
    transpose ((a,b),(c,d)) = ((a,c),(b,d))
    tr = arrHom transpose

instance Arrow Hom where
    arr = arrHom
    first = firstHom
