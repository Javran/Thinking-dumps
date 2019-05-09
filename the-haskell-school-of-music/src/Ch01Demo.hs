{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch01Demo where

import Euterpea


hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d = foldr (\p r -> hNote d p :+: r) (rest 0)

mel :: Pitch -> Pitch -> Pitch -> Music Pitch
mel p1 p2 p3 = hList qn [p1,p2,p3]

