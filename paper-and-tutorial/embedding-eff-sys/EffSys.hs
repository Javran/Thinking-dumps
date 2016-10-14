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
