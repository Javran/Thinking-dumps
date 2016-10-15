{-# LANGUAGE
    DataKinds
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
module EffSys where

import GHC.Exts

data Set (n :: [*]) where
    Empty :: Set '[]
    Ext :: e -> Set s -> Set (e ': s)

-- one pass in a buble sort, by the end of "Pass", we will sure the
-- last element in the resulting list is the "largest"
-- undering the corresponding {Min,Max} notion
type family Pass (l :: [*]) :: [*] where
    Pass '[] = '[]
    Pass '[e] = '[e]
    -- TODO: nested pattern requires UndecidableInstances (why?),
    -- not sure whether it is safe?
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

instance (Passer ((Max e f) ': s), OrdH e f) => Passer (e ': f ': s) where
    pass (Ext e (Ext f s)) = Ext (minH e f) (pass (Ext (maxH e f) s))

class OrdH e f where
    minH :: e -> f -> Min e f
    maxH :: e -> f -> Max e f

type family Nub t where
    Nub '[] = '[]
    Nub '[e] = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    Nub (e ': f ': s) = e ': Nub (f ': s)

class Nubable t where
    nub :: Set t -> Set (Nub t)

instance Nubable '[] where
    nub Empty = Empty

instance Nubable '[e] where
    nub (Ext x Empty) = Ext x Empty

instance (Nub (e ': f ': s) ~ (e ': Nub (f ': s)),
          Nubable (f ': s)) => Nubable (e ': f ': s) where
    nub (Ext e (Ext f s)) = Ext e (nub (Ext f s))

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

class Subset s t where
    subset :: Set t -> Set s

instance Subset '[] t where
    subset _ = Empty

instance Subset s t => Subset (x ': s) (x ': t) where
    subset (Ext x xs) = Ext x (subset xs)

instance Subset s t => Subset s (any ': t) where
    subset (Ext _ xs) = subset xs
