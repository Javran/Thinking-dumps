{-# LANGUAGE NoMonomorphismRestriction #-}
module TDPE where

-- Type-Directed Partial Evaluation

import TTF
import ToTDPE

data RR repr meta obj =
    RR { reify :: meta -> repr obj
       , reflect :: repr obj -> meta }

base :: RR repr (repr a) a
base = RR id id

infixr 8 -->

(-->) :: Semantics repr =>
         RR repr m1 o1 -> RR repr m2 o2 -> RR repr (m1 -> m2) (o1 -> o2)
r1 --> r2 = RR { reify = \m -> lam (reify r2 . m . reflect r1)
               , reflect = \o -> reflect r2 . app o . reify r1
               }
