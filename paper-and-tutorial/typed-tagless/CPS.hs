{-# LANGUAGE
    NoMonomorphismRestriction
  , TypeFamilies
  , ScopedTypeVariables
  #-}
module CPS where

-- http://okmij.org/ftp/tagless-final/course/CPS.hs

import TTF

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use fst" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

-- a "type function" that seems to translate a normal one into its CPS counterpart
type family CPSTypeTr (w :: *) (a :: *) :: *

type instance CPSTypeTr w Int = Int
type instance CPSTypeTr w Bool = Bool
type instance CPSTypeTr w Falsum = Falsum
type instance CPSTypeTr w (a -> b) = CPSTypeTr w a -> (CPSTypeTr w b -> w) -> w

data Falsum

newtype CPS repr w a =
    CPS { cpsr :: repr ((CPSTypeTr w a -> w) -> w) }

-- transforms a value into a lambda which feeds the value to whatever function
-- it is given.
-- note that the type of the actual representation is really what "CPSTypeTr w a" computes,
-- but the result is of type "CPS repr w a"

-- "cpsv v" creates a continuation, when called, feeds "v" as a result to whatever function
-- that wants it.
cpsv :: Semantics repr => repr (CPSTypeTr w a) -> CPS repr w a
cpsv v = CPS . lam $ \k -> app k v

-- this seems to just wrap a continuation inside datatype
cpsk :: Semantics repr => (repr (CPSTypeTr w a -> w) -> repr w) -> CPS repr w a
cpsk = CPS . lam

-- "appk e f" passes "e"'s result to "f", in which we capture the result value
-- and continue our computation
appk :: Semantics repr => CPS repr w a -> (repr (CPSTypeTr w a) -> repr w) -> repr w
appk (CPS e) f = app e (lam f)

-- CPS transformation
instance Semantics repr => Semantics (CPS repr w) where
    -- "Fischer & Plotkin's CPS transformation"
    -- call-by-value lambda calculus
    int x = cpsv $ int x
    -- we can also do:
    -- int x = cpsk $ \k -> app k (int x)
    -- also notice the difference between "cpsv" and "cpsk"'s implementation
    add e1 e2 = cpsk $ \k ->
        -- eval "e1" and get "v1" back
        appk e1 $ \v1 ->
        -- eval "e2" and get "v2" back
        appk e2 $ \v2 ->
          -- call "k" with the result
          app k (add v1 v2)
    lam e = cpsv $ lam (\x -> cpsr $ e (cpsv x))
    app ef ea = cpsk $ \k ->
        -- eval ef => vf
        appk ef $ \vf ->
        -- eval ea => va
        appk ea $ \va ->
          -- it's tempting to write "app k (app vf va)"
          -- because of the similarity between this and "add" case
          -- but that does not type check
          -- by looking at types of "vf" and "va", we are not getting
          -- the function type we want but continuation instead
          -- so here the only thing to do is applying "va" to "vf"
          -- and hope it will call "k" after the result is known.
          app (app vf va) k

-- so called "one-pass CPS transform", not sure what exactly is
-- administrative redices, for now I'm just getting the idea
-- of using meta-language to get rid of some kinds of boilerplates
newtype CPS1 repr w a =
    CPS1 { cps1r :: (repr (CPSTypeTr w a) -> repr w) -> repr w }

reflect :: Semantics repr =>
           ((repr a -> repr w) -> repr w) -> repr ((a -> w) -> w)
reflect e = lam (\k -> e (\v -> app k v))

cps1v :: repr (CPSTypeTr w a) -> CPS1 repr w a
cps1v v = CPS1 $ \k -> k v

instance Semantics repr => Semantics (CPS1 repr w) where
    int x = cps1v $ int x
    add e1 e2 = CPS1 $ \k ->
        cps1r e1 $ \v1 ->
        cps1r e2 $ \v2 ->
          k (add v1 v2)
    lam e = cps1v $ lam $ reflect . cps1r . e . cps1v
    app ef ea = CPS1 $ \k ->
        cps1r ef $ \vf ->
        cps1r ea $ \va ->
          app (app vf va) (lam k)

cps1 :: Semantics repr => CPS1 repr w a -> repr ((CPSTypeTr w a -> w) -> w)
cps1 = reflect . cps1r

{-
  transforming add (int 10) (int 20):
> unS (cpsr (add (int 10) (int 20) :: CPS S w Int) `app` lam id) 0
"((\\x0 -> ((\\x1 -> (x1 10)) (\\x1 -> ((\\x2 -> (x2 20)) (\\x2 -> (x0 (x1+x2))))))) (\\x0 -> x0))"
-}

{-
  examples given in the original source code:
-}

-- the "syntax tree" in final representation
te1 = th1
-- by using "cpsr", we make part of the type concrete (CPS)
-- and get the interpreted (transformed) result back
tec1 = cpsr te1
tec1_eval = eval tec1 id
-- 3
tec1_view = view tec1
-- "(\\x0 -> ((\\x1 -> (x1 1))
--    (\\x1 -> ((\\x2 -> (x2 2)) (\\x2 -> (x0 (x1+x2)))))))"

tek1 = cps1 te1
tek1_eval = eval tek1 id
-- 3
tek1_view = view tek1
-- "(\\x0 -> (x0 (1+2)))"

{-
  obviously "tek1_view" looks simpler than "tec1_view",
  I'm not sure what exactly is "administrative redices",
  but the following simplification seems to let us go from "tec1" to "tek1":

  ((\x -> x a) (\y -> <expr with y>)) => <expr with y replaced by a>

  in which "a" is a constant

  so "tec1" is basically:

  (\x -> ((\y -> y 1)
          (\y -> ((\z -> z 2)
                  (\z -> x (y + z))))))
  ==>
  (\x -> ((\y -> y 1)
          (x (y + 2)))) (innermost first)
  ==>
  (\x -> (x (1 + 2))) (innermost first)
-}
