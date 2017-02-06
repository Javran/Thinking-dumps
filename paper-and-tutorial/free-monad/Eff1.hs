{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Eff1 where

import OpenUnion5
import FTCQueue1

-- http://okmij.org/ftp/Haskell/extensible/Eff1.hs
-- built on top of OpenUnion5 & FTCQueue1

type Arrs r a b = FTCQueue (Eff r) a b

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
-}
qApp :: Arrs r b a -> b -> Eff r a
qApp q a = case tviewl q of
    TOne ab -> ab a
    ax :| q1 -> case ax a of
        Val x -> qApp q1 x
        E u q2 -> E u (q2 >< q1)
