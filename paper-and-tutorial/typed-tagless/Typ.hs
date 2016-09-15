{-# LANGUAGE RankNTypes, ExistentialQuantification, NoMonomorphismRestriction, PartialTypeSignatures #-}
module Typ where

-- trying to replicate http://okmij.org/ftp/tagless-final/course/Typ.hs

{-
  the language of type representation.
  supporting just integers and functions.
  concrete instances are needed to give semantics
-}
class TSYM trepr where
    tint :: trepr Int
    tarr :: trepr a -> trepr b -> trepr (a -> b)

-- "ShowT a" is for printing types out
newtype ShowT a = ShowT String

instance TSYM ShowT where
    tint = ShowT "Int"
    tarr (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ "->" ++ b ++ ")"

viewTy :: ShowT a -> String
viewTy (ShowT s) = s

-- TODO: I'm not sure what this is for, seems trivial.
newtype TQ t = TQ { unTQ :: forall trepr. TSYM trepr => trepr t }

-- TODO: seems the actual instance are hid inside TQ?
instance TSYM TQ where
    tint = TQ tint
    tarr (TQ a) (TQ b) = TQ (tarr a b)

data Typ = forall t. Typ (TQ t)

newtype EQU a b = EQU { equCast :: forall c. c a -> c b }

refl :: EQU a a
refl = EQU id

tran :: EQU a u -> EQU u b -> EQU a b
tran au ub = equCast ub au
-- consider turning (EQU a) u into (EQU a) b

-- "EQU _ b"
newtype FS b a = FS { unFS :: EQU a b }

-- "EQU a a" with first "a" changed to "b" by using "equ" to cast
symm :: forall a b. EQU a b -> EQU b a
symm equ = unFS . equCast equ . FS $ (refl :: EQU a a)

-- "EQU t (_ -> b)"
newtype F1 t b a = F1 { unF1 :: EQU t (a -> b) }

-- "EQU t (a -> _)"
newtype F2 t a b = F2 { unF2 :: EQU t (a -> b) }

eqArr :: EQU a1 a2 -> EQU b1 b2 -> EQU (a1 -> b1) (a2 -> b2)
eqArr a1a2 b1b2 = cast refl
  where
    cast = cast2 . cast1
    cast1 = unF1 . equCast a1a2 . F1
    cast2 = unF2 . equCast b1b2 . F2

data AsInt a = AsInt (Maybe (EQU a Int))

instance TSYM AsInt where
    tint = AsInt $ Just refl
    tarr _ _ = AsInt Nothing

asInt :: AsInt a -> c a -> Maybe (c Int)
asInt (AsInt (Just equ)) r = Just $ equCast equ r
asInt _ _ = Nothing

data AsArrow a =
    forall b1 b2. AsArrow (TQ a) (Maybe ((TQ b1,TQ b2), EQU a (b1 -> b2)))

-- TODO: maybe this should tell us what TQ is for?
instance TSYM AsArrow where
    tint = AsArrow tint Nothing
    tarr (AsArrow t1 _) (AsArrow t2 _) =
        AsArrow (tarr t1 t2) $ Just ((t1,t2),refl)

asArrow :: AsArrow a -> AsArrow a
asArrow = id

newtype SafeCast a = SafeCast (forall b. TQ b -> Maybe (EQU a b))

instance TSYM SafeCast where
    tint = SafeCast $ \tb ->
        case unTQ tb of
            AsInt eq -> fmap symm eq

    tarr (SafeCast t1) (SafeCast t2) = SafeCast $ \tb -> do
        AsArrow _ (Just ((b1,b2),equBb1b2)) <- pure (asArrow (unTQ tb))
        equT1b1 <- t1 b1
        equT2b2 <- t2 b2
        pure (tran (eqArr equT1b1 equT2b2) (symm equBb1b2))

safeGCast :: TQ a -> c a -> TQ b -> Maybe (c b)
safeGCast (TQ ta) ca tb = cast ta
  where
    cast (SafeCast f) = maybe Nothing (\equ -> Just (equCast equ ca)) (f tb)

data Dynamic = forall t. Dynamic (TQ t) t

-- the use of "NoMonomorphismRestriction" make it possible
-- to infer the most general type without restricting it to any concrete one.

-- GHC should be able to infer the following type
tt1 :: TSYM trepr => trepr ((Int -> Int) -> Int)
tt1 = (tint `tarr` tint) `tarr` tint

tdn1, tdn2, tdn3 :: Dynamic
tdn1 = Dynamic tint 5
tdn2 = Dynamic tt1 ($ 1)
tdn3 = Dynamic (tint `tarr` (tint `tarr` tint)) (*)
