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
