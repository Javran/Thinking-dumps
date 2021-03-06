{-# LANGUAGE
    ExistentialQuantification
  , NoMonomorphismRestriction
  , FlexibleContexts
  , ScopedTypeVariables
  , DataKinds
  , TypeOperators
  , RankNTypes
  , GADTs
  #-}
module Eff1 where

import OpenUnion5
import FTCQueue1

import Control.Monad

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

instance Applicative (Eff r) where
    pure = Val
    mf <*> mx = case mf of
        Val f -> case mx of
            Val x -> Val (f x)
            E ux qx ->
                -- basically we never touch already existing parts,
                -- and append "things to be done" to the list
                E ux (qx |> (Val . f))
        E uf qf -> case mx of
            Val x -> E uf (qf |> (Val . ($ x)))
            m -> E uf (qf |> (<$> m))

instance Monad (Eff r) where
    mx >>= mf = case mx of
        Val x -> mf x
        E ux qx -> E ux (qx |> mf)

-- request computation "t" of return value "v", and wrap the result
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)

{-
note that "Union [] a" cannot be constructed:
the only thing that constructs a Union is through "inj", which requires you
to give it something.
therefore the case for "E _ _" is unreachable.
-}
run :: Eff '[] w -> w
run (Val x) = x
run (E _ _) = error "run: unreachable code"

{-# ANN handleOrRelay "HLint: ignore Eta reduce" #-}
handleOrRelay :: (a -> Eff r w) -- return
              -> (forall v. t v -> Arr r v w -> Eff r w) -- handle
              -> Eff (t ': r) a -- input Eff
              -> Eff r w -- output Eff, notice that "t" is removed
handleOrRelay ret h m = loop m
  where
    -- a value is simply returned by passing it to "ret"
    loop (Val x) = ret x
    -- there are 2 outcomes of destructing the "effect set":
    loop (E u q) = case decomp u of
        -- either we can find the desired effect, in which case we handle it
        Right x -> h x (q `qComp` loop)
        -- or the desired effect is missing, in which case the type is "refined" (to "u'")
        -- to exclude that effect, and the request is relayed
        Left u' -> E u' (tsingleton (q `qComp` loop))

{-

notice the difference between "handleOrRelay" and "interpose".
one uses "decomp" and another uses "prj" ..
(TODO) I'm not sure what exactly does "interpose" mean ..
-}

{-# ANN interpose "HLint: ignore Eta reduce" #-}
interpose :: Member t r
          => (a -> Eff r w) -- return
          -> (forall v. t v -> Arr r v w -> Eff r w) -- handle
          -> Eff r a -- input Eff
          -> Eff r w
interpose ret h m = loop m
  where
    loop (Val x) = ret x
    loop (E u q) = case prj u of
        Just x -> h x (q `qComp` loop)
        Nothing -> E u (tsingleton (q `qComp` loop))

data Reader e v where
    Reader :: Reader e e

-- this type signature can be inferred when NoMonomorphismRestriction is on
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

{-
notice the "\Reader k -> k e",
this is *not* "\(Reader k) -> k e": as the latter is an invalid destruction because
"Reader" has no fields. but the former is a function that takes 2 arguments:
a "Reader" that does nothing on value level, but guides type inference as GADT
holds the information that "e ~ v"

also notice that the Eff changes from "Eff (Reader e ': r) w" to "Eff r w"
so one outermost layer of the effect is handled and removed from the effect list.
-}
runReader :: Eff (Reader e ': r) w -> e -> Eff r w
runReader m e = handleOrRelay pure (\(Reader :: Reader e v) k -> k e) m

-- run computation within a modified environment.
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
    e <- f <$> ask
    let h :: Reader e v -> Arr r v a -> Eff r a
        h Reader g = g e
    interpose pure h m

-- type can be inferred.
t1 :: Member (Reader Int) r => Eff r Int
t1 = (+) <$> ask <*> pure (1 :: Int)

t1r :: Int
t1r = run (runReader t1 (20 :: Int))
