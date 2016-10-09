{-# LANGUAGE
    NoMonomorphismRestriction
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}
module LinearLC where

-- http://okmij.org/ftp/tagless-final/course/LinearLC.hs

import Prelude hiding (any)

{-# ANN module "HLint: ignore Use &&&" #-}

newtype F a = F a
data U = Used

second :: (b->b') -> (a,b) -> (a,b')
second = fmap

first :: (a->a') -> (a,b) -> (a',b)
first f ~(x,y) = (f x,y)

class LSemantics repr where
    -- notice that unlike "repr h a" which is what we usually see,
    -- we now have two "environments", "repr hi ho a", which gives us
    -- distinction between "input environment" and "output environment"

    -- using a constant does not change our environment
    int :: Int -> repr hi hi Int
    -- "add <a> <b>" first traverses "<a>" with "hi" as input env and "h" as intermedia env
    -- and then "<b>" with "h" as input env and "ho" output env
    -- forming an expression that takes as input env "hi" and turns it into "ho"
    add :: repr hi h Int -> repr h ho Int -> repr hi ho Int

    -- a reference about the variable consumes that variable from environment.
    z :: repr (F a, h) (U, h) a
    -- "s <e>" hides the first element for "<e>"
    s :: repr hi ho a -> repr (any,hi) (any,ho) a
    -- actually this case is very similar to "add": for "app <a> <b>",
    -- we first traverse "<a>" then "<b>", and finally do the application
    app :: repr hi h (a->b) -> repr h ho a -> repr hi ho b

-- "lam" is separated because in this implementation "hi" and "ho" needs to present
-- in class head.
class LinearL repr hi ho where
    lam :: repr (F a, hi) (U, ho) b -> repr hi ho (a->b)

class HiHo hi ho where
    hiho :: hi -> ho

instance HiHo () () where
    hiho = id

instance HiHo hi ho => HiHo (F a, hi) (F a, ho) where
    hiho = second hiho

instance HiHo hi ho => HiHo (U, hi) (U, ho) where
    hiho = second hiho

instance HiHo hi ho => HiHo (F a, hi) (U, ho) where
    hiho = second hiho . first (const Used)

-- note that there isn't (and really shouldn't be) a case
-- for "HiHo (U, hi) (F a, ho)", as it does not make sense for a linear system.

newtype R hi ho a = R { unR :: hi -> (a,ho) }

instance HiHo hi ho => LinearL R hi ho where
    lam (R e) = R $ \hi -> (f hi, hiho hi)
      where
        f hi x | (v,_) <- e (F x, hi) = v

instance LSemantics R where
    int x = R $ \hi -> (x,hi)
    add = liftBinOp (+)
    z = R $ \(F x, h) -> (x, (Used, h))
    s (R v) = R $ \(any, hi) ->
        let (x,ho) = v hi
        in (x,(any,ho))
    app = liftBinOp ($)

-- to show that "add" and "app" are just the same thing parameterized by
-- different functions.
liftBinOp :: (a->b->c) -> R hi h a -> R h ho b -> R hi ho c
liftBinOp f (R e1) (R e2) = R $ \hi ->
    let (v1,h) = e1 hi
        (v2,ho) = e2 h
    in (v1 `f` v2, ho)

-- evaluator taking input env "()"
-- while the output env can be open.
-- I believe we can set "b ~ ()" to make the evaluator
-- only accept closed terms
eval :: R () b a -> a
eval (R e) = fst $ e ()

newtype S hi ho a = S { unS :: [String] -> String }

instance LSemantics S where
    int x = S $ \_ -> show x
    add (S e1) (S e2) = S $ \h ->
        "(" ++ e1 h ++ "+" ++ e2 h ++ ")"
    z = S $ \(x:_) -> x
    s (S v) = S $ \(_:h) -> v h
    app (S e1) (S e2) = S $ \h ->
        "(" ++ e1 h ++ " " ++ e2 h ++ ")"

instance LinearL S hi ho where
    lam (S e) = S $ \h ->
        let x = "x" ++ show (length h)
        in "(\\!" ++ x ++ " -> " ++ e (x:h) ++ ")"

-- only closed terms can be viewed
view :: S () () a -> String
view (S e) = e []

-- closed term, feeding it to "eval" causes "h ~ ()",
-- but this term itself can still be one part of a bigger syntax tree.
tl1 :: LSemantics repr => repr h h Int
tl1 = add (int 1) (int 2)

-- open term, which requires a Int resource (by "s z") waiting to be used.
tl2o :: (LSemantics repr, LinearL repr (F Int, h) (U, h))
        => repr (F Int, h) (U, h) (Int -> Int)
tl2o = lam (add z (s z))

tl3 :: (LSemantics repr, LinearL repr h h) => repr h h ((Int -> Int) -> Int)
tl3 = lam (add (app z (int 1)) (int 2))

-- ignoring the constraints, the term is closed and typed "Int -> Int -> Int".
-- TODO: not sure if the constraint is intended, with "z" and "s z",
-- shouldn't we have more constraints?
tl4 :: ( LSemantics repr
       , LinearL repr h h
       , LinearL repr (F Int,h) (U,h)
       ) => repr h h (Int -> Int -> Int)
tl4 = lam (lam (add z (s z)))

-- for non-linear lambdas
newtype G a = G a

class GenL repr hi ho where
    glam :: repr (G a, hi) (G a, ho) b -> repr hi ho (a->b)

class GZ repr where
    gz :: repr (G a, hi) (G a, hi) a

instance HiHo hi ho => GenL R hi ho where
    glam (R e) = R $ \hi -> (f hi, hiho hi)
      where
        f hi x = let (v,_) = e (G x, hi)
                 in v

instance GZ R where
    gz = R $ \p@(G x, _) -> (x, p)

instance HiHo hi ho => HiHo (G a, hi) (G a, ho) where
    hiho (x,hi) = (x, hiho hi)

instance GZ S where
    gz = S $ \(x:_) -> x

instance GenL S hi ho where
    glam (S e) = S $ \h ->
        let x = "y" ++ show (length h)
        in "(\\" ++ x ++ " -> " ++ e (x:h) ++ ")"

-- "gz" is a resource (variable) that can be used more than once.
tg2 :: (LSemantics repr, GenL repr h h, GZ repr) => repr h h (Int -> Int)
tg2 = glam (add gz gz)

-- "tgk" (the K combinator) is the constant function with ignores its first argument.
-- it's not linear because the resource should be consumed exactly once.
tgk :: (GenL repr h h, GenL repr (G a, h) (G a, h), GZ repr) => repr h h (a -> b -> b)
tgk = glam (glam gz)

-- another K combinator, but with its second argument being linear.
-- semantically different than "tgk"
tgk' :: ( LinearL repr (G a, h) (G a, h)
        , LSemantics repr
        , GenL repr h h) => repr h h (a -> b -> b)
tgk' = glam (lam z)

-- I think this could be helpful if we can see how the type signature involves
-- as the expression gets more complicated

{-
  "lam" is defined for "LinearL repr h h", which exposes input and output environment
  in the header of a constraint,
  and all other components of a linear language comes from "LSemantics"

  - tg51's type signature isn't saying much: just that we expect an environment
    and that this expression preserves that environment.

-}
tg51 :: ( LinearL repr h h, LSemantics repr ) => repr h h (b -> b)
tg51 = lam z

-- "h" in tg51's type further refines to "(G b, h)", through the presence of "gz"
tg52 :: ( LinearL repr (G b, h) (G b, h), LSemantics repr
        , GZ repr) => repr (G b, h) (G b, h) b
tg52 = app tg51 gz

-- "glam" "simplifies" the type by hiding the body
tg5 :: ( LinearL repr (G b, h) (G b, h), LSemantics repr
       , GZ repr
       , GenL repr h h) => repr h h (b -> b)
tg5 = glam tg52

-- tg5 = glam (app (lam z) gz)
