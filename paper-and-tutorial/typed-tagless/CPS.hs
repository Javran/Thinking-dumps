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
cpsv :: Semantics repr => repr (CPSTypeTr w a) -> CPS repr w a
cpsv v = CPS . lam $ \k -> app k v

-- this seems to just wrap a continuation inside datatype
cpsk :: Semantics repr => (repr (CPSTypeTr w a -> w) -> repr w) -> CPS repr w a
cpsk = CPS . lam

appk :: Semantics repr => CPS repr w a -> (repr (CPSTypeTr w a) -> repr w) -> repr w
appk (CPS e) f = app e (lam f)

-- CPS transformation
instance Semantics repr => Semantics (CPS repr w) where
    int x = cpsv $ int x
    add e1 e2 = cpsk $ \k ->
        appk e1 $ \v1 ->
        appk e2 $ \v2 ->
          app k (add v1 v2)
    lam e = cpsv $ lam (\x -> cpsr $ e (cpsv x))
    app ef ea = cpsk $ \k ->
        appk ef $ \vf ->
        appk ea $ \va ->
          app (app vf va) k
