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
    tbool :: trepr Bool
    tarr :: trepr a -> trepr b -> trepr (a -> b)

-- "ShowT a" is for printing types out
newtype ShowT a = ShowT String

instance TSYM ShowT where
    tint = ShowT "Int"
    tbool = ShowT "Bool"
    tarr (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ "->" ++ b ++ ")"

viewTy :: ShowT a -> String
viewTy (ShowT s) = s

-- search for "existential typeclass", basically this is a possible way
-- to hide instance inside and making it still polymorphic.
-- (I could be wrong, still unsure of some concepts)
newtype TQ t = TQ { unTQ :: forall trepr. TSYM trepr => trepr t }

instance TSYM TQ where
    tint = TQ tint
    tbool = TQ tbool
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
    tbool = AsInt Nothing
    tarr _ _ = AsInt Nothing

asInt :: AsInt a -> c a -> Maybe (c Int)
asInt (AsInt (Just equ)) r = Just $ equCast equ r
asInt _ _ = Nothing

data AsBool a = AsBool (Maybe (EQU a Bool))

instance TSYM AsBool where
    tint = AsBool Nothing
    tbool = AsBool $ Just refl
    tarr _ _ = AsBool Nothing

asBool :: AsBool a -> c a -> Maybe (c Bool)
asBool (AsBool (Just equ)) r = Just (equCast equ r)
asBool _ _ = Nothing

data AsArrow a =
    forall b1 b2. AsArrow (TQ a) (Maybe ((TQ b1,TQ b2), EQU a (b1 -> b2)))

instance TSYM AsArrow where
    tint = AsArrow tint Nothing
    tbool = AsArrow tbool Nothing
    tarr (AsArrow t1 _) (AsArrow t2 _) =
        AsArrow (tarr t1 t2) $ Just ((t1,t2),refl)

asArrow :: AsArrow a -> AsArrow a
asArrow = id

newtype SafeCast a = SafeCast (forall b. TQ b -> Maybe (EQU a b))

instance TSYM SafeCast where
    tint = SafeCast $ \tb ->
        case unTQ tb of
            AsInt eq -> fmap symm eq
    tbool = SafeCast $ \tb ->
        case unTQ tb of
            AsBool eq -> fmap symm eq
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

tt2 :: TSYM trepr => trepr (Int -> Int -> Int)
tt2 = tint `tarr` (tint `tarr` tint)

tdn1, tdn2, tdn3 :: Dynamic
tdn1 = Dynamic tint 5
tdn2 = Dynamic tt1 ($ 1)
tdn3 = Dynamic (tint `tarr` (tint `tarr` tint)) (*)

{-
  observation:

- basically doing the same thing:

> viewTy tint
"Int"
> viewTy (unTQ tint)
"Int"
> viewTy (unTQ (unTQ tint))
"Int"
> viewTy (unTQ (unTQ (unTQ tint)))
"Int"

my guess is by doing so we are delaying the choice of instance
until "unTQ" destruction.

- trying safeGCast function:

> isJust (safeGCast tint (Identity 1) (tint `tarr` tint))
False
> isJust (safeGCast tint [1,3,4,5] (tint `tarr` tint))
False
> runIdentity (fromJust (safeGCast (tint `tarr` tint) (Identity (+ 20)) (tint `tarr` tint))) 5
25

-}

data TCOPY trep1 trep2 a = TCOPY (trep1 a) (trep2 a)

{-
  it seems "Q" in "TQ" means quantification, and now we have "TCOPY"
  which looks like an alternative to it.
  I guess after all this is just a type representation,
  unlike values that we can do all sorts of things: printing, interpreting, there are simply
  not much we can do with types. but I do suspect this makes a difference
  when we are trying to do multiple things with "tt1 :: TSYM trepr => trepr a",
  which would force "trepr" be either (but not both)
-}
instance (TSYM trep1, TSYM trep2) => TSYM (TCOPY trep1 trep2) where
    tint = TCOPY tint tint
    tbool = TCOPY tbool tbool
    tarr (TCOPY a1 a2) (TCOPY b1 b2) = TCOPY (a1 `tarr` b1) (a2 `tarr` b2)

newtype Id a = Id a

tdnEval1 :: Dynamic -> (Int -> Int) -> Maybe String
tdnEval1 (Dynamic tr d) x = do
    -- tt1 is basically (Int -> Int) -> Int, so we are expecting
    -- "d" to be of type tt1, and apply "x" to it.
    -- as we are in the Maybe monad, we'll get Nothing
    -- if something goes wrong
    Id f <- safeGCast tr (Id d) tt1
    pure (show (f x))

tdnEval2 :: Dynamic -> Int -> Int -> Maybe String
tdnEval2 (Dynamic tr d) x y = do
    Id f <- safeGCast tr (Id d) tt2
    pure (show (f x y))

data ShowAs a = ShowAs (TQ a) (a -> String)

instance TSYM ShowAs where
    tint = ShowAs tint show
    tbool = ShowAs tbool show
    tarr (ShowAs t1 _) (ShowAs t2 _) =
        ShowAs t (\_ -> "<function of the type " ++
                        viewTy (unTQ t) ++ ">")
      where
        t = tarr t1 t2

showAs :: TQ a -> a -> String
showAs tr a = case unTQ tr of
    ShowAs _ f -> f a
