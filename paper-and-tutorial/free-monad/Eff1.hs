{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Eff1 where

import OpenUnion5
import FTCQueue1

-- http://okmij.org/ftp/Haskell/extensible/Eff1.hs
-- built on top of OpenUnion5 & FTCQueue1

{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

type Arrs r a b = FTCQueue (Eff r) a b
type Arr r a b = a -> Eff r b

data Eff r a
  = Val a -- done with the value of type "a"
  | forall b. E (Union r b) (Arrs r b a) -- request "b", with continuation saved

{-
- not going to do inline and that kind of stuff before we know what we are doing...
- basically the implementation sounds like maintaining a chain of computations that need to happen
  in order, and we always look at the left-most (first) one, run it, and figure out how to continue:

  - if the computation yields a value, we'll just take that value and feed it to the next computation in chain
  - TODO: I'm not sure about the "u" part of "E u q2" below. "q2" however is definitely the continuation
    that we do right after request is handled (so it's pushed in front of everything)
  - notice that "r" doesn't change, which is the "universe" of effects we are dealing with.
-}
qApp :: Arrs r b a -> b -> Eff r a
qApp q a = case tviewl q of
    TOne ab -> ab a
    ax :| q1 -> case ax a of
        Val x -> qApp q1 x
        E u q2 -> E u (q2 >< q1)

-- "h" post-composed on outcome of "g", given "a" as input.
-- note that "h" can change the set of effects besides transforming some kind of "b" into "c"
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h = \a -> h (qApp g a)

-- basically just "qComp" ... but I think we need to figure out
-- the difference between "Arrs" and "Arr" eventually
qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = tsingleton (qComp g h)

instance Functor (Eff r) where
    fmap f (Val x) = Val (f x)
    -- sounds like we are not actually doing the mapping if we have not yet come to a value
    -- in this case "f" is not even touch: it's simply carried around
    fmap f (E u q) = E u (q |> (Val . f))
