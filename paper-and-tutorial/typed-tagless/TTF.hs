{-# LANGUAGE NoMonomorphismRestriction #-}
module TTF where

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use fst" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

-- http://okmij.org/ftp/tagless-final/course/TTF.hs
class Semantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    lam :: (repr a -> repr b) -> repr (a->b)
    app :: repr (a->b) -> repr a -> repr b

newtype R a = R {unR :: a}

instance Semantics R where
    int x     = R x
    add (R e1) (R e2)= R $ e1 + e2
    lam f = R $ unR . f . R
    app (R e1) (R e2) = R $ e1 e2

eval :: R a -> a
eval = unR

type VarCounter = Int
newtype S a = S {unS:: VarCounter -> String}

instance Semantics S where
    int x = S $ const $ show x
    add (S e1) (S e2) = S $ \h ->
        "(" ++ e1 h ++ "+" ++ e2 h ++ ")"

    lam e = S $ \h ->
       let x = "x" ++ show h
       in "(\\" ++ x ++ " -> " ++
            unS (e (S $ const x)) (succ h) ++ ")"
    app (S e1) (S e2) = S $ \h ->
      "(" ++ e1 h ++ " " ++ e2 h ++ ")"

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
    fix :: (repr a -> repr a) -> repr a

instance MulSYM S where
    mul (S e1) (S e2) = S $ \h ->
      "(" ++ e1 h ++ "*" ++ e2 h ++ ")"

instance BoolSYM S where
    bool x = S $ const $ show x
    leq (S e1) (S e2) = S $ \h ->
      "(" ++ e1 h ++ "<=" ++ e2 h ++ ")"
    if_ (S be) (S et) (S ee) = S $ \h ->
       unwords["(if", be h, "then", et h, "else", ee h,")"]

instance FixSYM S where
    fix e = S $ \h ->
       let self = "self" ++ show h
       in "(fix " ++ self ++ "." ++
            unS (e (S $ const self)) (succ h) ++ ")"
