{-# LANGUAGE
    DataKinds
  , KindSignatures
  , TypeOperators
  , GADTs
  , TypeFamilies
  , PolyKinds
  , UndecidableInstances
  #-}
module EffSys where

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

type family Nub t where
    Nub '[] = '[]
    Nub '[e] = '[e]
    Nub (e ': e ': s) = Nub (e ': s)
    Nub (e ': f ': s) = e ': Nub (f ': s)
