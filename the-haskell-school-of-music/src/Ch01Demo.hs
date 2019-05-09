{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch01Demo where

import Euterpea

{- Ex 1.4 for adding interval as argument -}

hNote :: Int -> Dur -> Pitch -> Music Pitch
hNote inv d p = note d p :=: note d (trans inv p)

hList :: Int -> Dur -> [Pitch] -> Music Pitch
hList inv d = foldr (\p r -> hNote inv d p :+: r) (rest 0)

mel :: Pitch -> Pitch -> Pitch -> Music Pitch
mel p1 p2 p3 = hList (-3) qn [p1,p2,p3]

