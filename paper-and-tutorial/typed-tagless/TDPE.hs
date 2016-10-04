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
{-

bbf1 :: a -> b -> a

the only function (ignoring "undefined" and non-terminating terms)
that makes sense is "bbf1 = \x -> \y -> x"

bbf2 :: (c -> c) -> c -> c

we actually have 2 options for this:

1. bbf2 f x = f x

2. bbf2 _ x = x
   (this one actually has a more general type:
    bbf2 :: b -> a -> a)

for the next part I don't get it why reify_aa :: (a -> a) -> repr (a -> a)
is actually reify_aa (repr a -> repr a) -> repr (a -> a).
when we assign a := repr a,
shouldn't we also change "repr (a -> a)" to "repr (repr a -> repr a)"?

what if:

reify_aa :: (forall a. a -> a) -> forall a. repr (a -> a) ?

or:

reify_aa :: forall b. (forall a. a -> a) -> repr (b -> b)

let a ~ repr a

we'll have:

reify_aa :: forall b. (repr a -> repr a) -> repr (b -> b)

so if we fix b ~ a, we'll have:

reify_aa :: (repr a -> repr a) -> repr (a -> a)

which we have already seen one of the functions that has this type:

reify_aa = lam

-}
