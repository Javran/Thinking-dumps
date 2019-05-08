module Ch01Demo where

import Euterpea

mel :: Pitch -> Pitch -> Pitch -> Music Pitch
mel p1 p2 p3 =
    (nq p1 :=: nq (tm3 p1))
    :+: (nq p2 :=: nq (tm3 p2))
    :+: (nq p3 :=: nq (tm3 p3))
  where
    nq = note qn
    tm3 = trans (-3)
