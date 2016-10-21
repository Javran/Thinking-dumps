{-# LANGUAGE
    DataKinds
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , RankNTypes
  , RebindableSyntax
  , FlexibleContexts
  , ConstraintKinds
  , MultiParamTypeClasses
  , FlexibleInstances
  , KindSignatures
  , TypeOperators
  , GADTs
  , TypeFamilies
  , PolyKinds
  , UndecidableInstances
  #-}
module TypeLevelSets where

import Prelude hiding (return, pure, (>>=), (>>))
import GHC.Exts

-- related: https://hackage.haskell.org/package/type-level-sets

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

-- one pass in a buble sort, by the end of "Pass", we will sure the
-- last element in the resulting list is the "largest"
-- undering the corresponding {Min,Max} notion
type family Pass (l :: [*]) :: [*] where
    Pass '[] = '[]
    Pass '[e] = '[e]
    -- nested pattern requires UndecidableInstances:
    -- https://downloads.haskell.org/~ghc/8.0.1-rc4/docs/html/users_guide/glasgow_exts.html#instance-declarations
    -- basically there are few rules that GHC uses to ensure instance resolution terminates
    -- with "UndecidableInstances", these rules will be bypassed so
    -- now we are responsible to make sure that instance resolution does terminates
    -- in this example the input is "(e : f : s)" and the recursive call is "(_ : s)" where
    -- "_" is one of either "e" or "f".
    -- so yes this does guarantee to be a "smaller problem" so this is safe.

    -- but this is a type family definition,
    -- how is this related to undecidable *instances* ?
    -- https://downloads.haskell.org/~ghc/8.0.1-rc4/docs/html/users_guide/glasgow_exts.html#type-family-decidability
    -- so on the RHS of the following definition:
    -- - (1) "(Pass ((Max e f) ': s))" is a type family application, and its argument contains
    --   another type family constructor "Max"
    -- - (2) also, for the same part, type variable e,f,s
    --   is not strictly smaller than e,f,s (on the LHS)
    -- - (3) this one is satisfied
    Pass (e ': f ': s) = Min e f ': (Pass ((Max e f) ': s))

-- Min and Max are sorting mechanism
type family Min (a :: k) (b :: k) :: k
type family Max (a :: k) (b :: k) :: k

type family Bubble l l' where
    -- note that here "l'" is used as a list length counter:
    -- when the counter reaches 0 ("'[]"), we stop applying it to "Pass"
    Bubble l '[] = l
    -- when the counter is non-zero (guaranteed by having (_ ': _) pattern
    -- we recursively deal with "Bubble l xs" first, and then apply a "Pass"
    -- to the resulting type
    Bubble l (x ': xs) = Pass (Bubble l xs)
{- say if we have a type level list of size 3, [a,b,c]:

   Bubble [a,b,c] [a,b,c]
=> Pass (Bubble [a,b,c] [b,c])
=> Pass (Pass (Bubble [a,b,c] [b]))
=> Pass (Pass (Pass (Bubble [a,b,c] [])))
=> Pass (Pass (Pass [a,b,c]))

so if the list length is n, we will have n passes applications "Pass"
-}

-- the interface to bubble sort, the output won't
-- always be correct if Bubble's two argument list mismatch in length
type Sort s = Bubble s s

-- TODO: the purpose of having both type-level and value-level stuff?
type Sortable s = Bubbler s s
class Bubbler s s' where
    bubble :: Set s -> Set s' -> Set (Bubble s s')

instance Bubbler s '[] where
    bubble s Empty = s

instance (Bubbler s t, Passer (Bubble s t)) => Bubbler s (e ': t) where
    bubble s (Ext _ t) = pass (bubble s t)

class Passer s where
    pass :: Set s -> Set (Pass s)

instance Passer '[] where
    pass Empty = Empty

instance Passer '[e] where
    pass (Ext e Empty) = Ext e Empty
    pass _ = error "impossible"

instance (Passer ((Max e f) ': s), OrdH e f) => Passer (e ': f ': s) where
    pass (Ext e (Ext f s)) = Ext (minH e f) (pass (Ext (maxH e f) s))
    pass _ = error "impossible"

class OrdH e f where
    minH :: e -> f -> Min e f
    maxH :: e -> f -> Max e f

type family Nub t where
    Nub '[] = '[]
    Nub '[e] = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    -- note that this case overlaps with the previous one.
    -- (with previous one "(e ': e ': s)" being more difficult to satisfy)
    -- only attempted when all other pattern have failed.
    Nub (e ': f ': s) = e ': Nub (f ': s)

class Nubable t where
    nub :: Set t -> Set (Nub t)

instance Nubable '[] where
    nub Empty = Empty

instance Nubable '[e] where
    nub (Ext x Empty) = Ext x Empty
    nub _ = error "impossible"
{-
  not mentioned in the paper, but "{-# OVERLAPS #-}" seems to does the trick:
  OVERLAPS is both OVERLAPPABLE and OVERLAPPING.
  - OVERLAPPABLE: allowing a instance to be overlapped by others
  - OVERLAPPING: expect this instance to overlap others

  TODO: details are in:
  https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-overlap
-}
instance {-# OVERLAPS #-} (Nub (e ': f ': s) ~ (e ': Nub (f ': s)),
          Nubable (f ': s)) => Nubable (e ': f ': s) where
    nub (Ext e (Ext f s)) = Ext e (nub (Ext f s))
    nub _ = error "impossible"

type AsSet s = Nub (Sort s)

asSet :: (Sortable s, Nubable (Sort s)) => Set s -> Set (AsSet s)
asSet x = nub (bsort x)

-- TODO: not sure whether this is correct, but it type checks.
bsort :: Bubbler s s => Set s -> Set (Sort s)
bsort s = bubble s s

type IsSet s = ((s ~ Nub (Sort s)) :: Constraint)

type family Append (s :: [*]) (t :: [*]) :: [*] where
    Append '[] t = t
    Append (x ': xs) ys = x ': Append xs ys

append :: Set s -> Set t -> Set (Append s t)
append Empty x = x
append (Ext e xs) ys = Ext e (append xs ys)

type Union s t = AsSet (Append s t)
type Unionable s t = (Sortable (Append s t), Nubable (Sort (Append s t)))

union :: (Unionable s t) => Set s -> Set t -> Set (Union s t)
union s t = nub (bsort (append s t))

-- "Subset s t" is a valid instance iff. s is a subset of t
class Subset s t where
    subset :: Set t -> Set s

instance Subset '[] t where
    subset _ = Empty

instance Subset s t => Subset (x ': s) (x ': t) where
    subset (Ext x xs) = Ext x (subset xs)

instance Subset s t => Subset s (any ': t) where
    subset (Ext _ xs) = subset xs
-- direction: input "st", output "s" "t"
class Split s t st where
    split :: Set st -> (Set s, Set t)

instance Split '[] '[] '[] where
    split _ = (Empty, Empty)

-- distributing an element to both
instance {-# OVERLAPPABLE #-} (Split s t st) => Split (x ': s) (x ': t) (x ': st) where
    split (Ext x st) = (Ext x s, Ext x t)
      where
        (s,t) = split st

-- distributing to just "s" set
instance {-# OVERLAPS #-} (Split s t st) => Split (x ': s) t (x ': st) where
    split (Ext x st) = (Ext x s, t)
      where
        (s,t) = split st

-- distributing to just "t" set
instance {-# OVERLAPS #-} (Split s t st) => Split s (x ': t )(x ': st) where
    split (Ext x st) = (s, Ext x t)
      where
        (s,t) = split st

{-
  shouldn't we have something like:

  "instance (Split s t st) => Split s t (any : st)" ?

  one possible way of thinking this: note that we are forced to give
  the exact list of things we might want to read from. so that
  whenever some type appears in "st", it must be an element of either "s" or "t"

  additionally, we have the following constraint when composing Readers:
  "type Inv Reader s t = (IsSet s, IsSet t, Split s t (Union s t))"
  which explictly specifies that
  the set we are spliting must be the union of "s" and "t"
-}
