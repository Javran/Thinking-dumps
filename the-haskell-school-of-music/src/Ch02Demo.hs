module Ch02Demo where

import Euterpea

t251 :: Music Pitch
t251 = dMinor :+: gMajor :+: cMajor
  where
    dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
    gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
    cMajor = c 4 bn :=: e 4 bn :=: g 4 bn

