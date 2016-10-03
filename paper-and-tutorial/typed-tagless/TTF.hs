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

view :: S a -> String
view e = unS e 0

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
    fix :: (repr a -> repr a) -> repr a

instance MulSYM R where
    mul e1 e2 = R $ unR e1 * unR e2

instance BoolSYM R where
    bool b    = R b
    leq e1 e2 = R $ unR e1 <= unR e2
    if_ be et ee = R $ if unR be then unR et else unR ee

instance FixSYM R where
    fix f = R $ fx (unR . f . R) where fx f' = f' (fx f')

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

-- examples:
th1 = add (int 1) (int 2)
th2 = lam (\x -> add x x)
th3 = lam (\x -> add (app x (int 1)) (int 2))

th1_eval = eval th1
th2_eval = eval th2
th2_eval' = eval th2 21
th3_eval = eval th3

th1_view = view th1
th2_view = view th2
th3_view = view th3

tpow = lam (\x -> fix (\self -> lam (\n ->
                                     if_ (leq n (int 0)) (int 1)
                                     (mul x (app self (add n (int (-1))))))))

tpow7 = lam (\x -> app (app tpow x) (int 7))
tpow72 = app tpow7 (int 2)

tpow_eval = eval tpow
tpow72_eval = eval tpow72
tpow_view = view tpow
