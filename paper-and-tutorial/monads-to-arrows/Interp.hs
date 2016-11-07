{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Interp where

import Control.Arrow
import Data.Maybe
import Data.Functor.Identity
import qualified Control.Category as Cat

newtype AId i o = AId { runAId :: i -> Identity o }

instance Cat.Category AId where
    id = AId Identity
    (AId g) . (AId f) = AId (g . runIdentity . f)

instance Arrow AId where
    arr f = AId (Identity . f)
    first (AId f) = AId (Identity . first (runIdentity . f))

instance ArrowChoice AId where
    left (AId f) = AId (Identity . left (runIdentity . f))

instance ArrowApply AId where
    app = AId (Identity . \(AId f, x) -> runIdentity (f x))

data Exp
  = Var String
  | Add Exp Exp
  | If Exp Exp Exp
  | Lam String Exp
  | App Exp Exp

data Val
  = VNum Int
  | VBl Bool
  | VFn (AId Val Val)

type Env = [(String,Val)]

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

eval :: Exp -> AId Env Val
eval (Var s) = arr (fromJust . lookup s)
eval (Add e1 e2) = liftA2 add (eval e1) (eval e2)
  where
    add (VNum x) (VNum y) = VNum (x+y)
    add _ _ = error "expecting numbers"
eval (If eIf eThen eElse) =
    -- step 1: evaluate eIf and convert resulting value
    test (eval eIf >>> arr (\(VBl b) -> b)) >>>
    -- step 2: now we can dispatch accordingly
    eval eThen ||| eval eElse
eval (Lam x e) = arr (\env ->
                      -- manipulate env for binding the new variable
                      VFn (arr (\v -> (x,v) : env) >>>
                           -- then the body is evaluated
                           eval e))
eval (App eF eA) =
    -- 1. evaluate both function and its argument
    (eval eF &&& eval eA) >>>
    -- 2. extract the function from result
    -- note the definition of "Val": the "VFn" case accepts
    -- an haskell function as argument, (i.e. Val -> Val),
    -- but this is too specific, in general we know nothing
    -- more than that it acts like an arrow, that's why we
    -- want to use "app" here. (TODO: so we have to change this)
    first (arr (\(VFn f) -> f)) >>>
    app

-- (->) is an instance of Arrow, which means the following code does typecheck
eval' :: Exp -> Env -> Val
eval' e = runIdentity . runAId (eval e)

{-
  ArrowChoice offers us:

  left :: b ~~> c -> (Either b d ~~> Either c d)

  so:
  - if the input is "Left _", it will be fed to that argument arrow.
  - if the input is "Right _", it's left unchanged.

  this is the primitive to make it possible for doing things conditionally
-}

{-
  think about: test :: (b ~~> Bool) -> (b ~~> Either b b)
  it turns an arrow that outputs boolean values
  into an arrow that keeps its input and encode the boolean value
  through "Either". note that the input is kept this way
  instead of being consumed
-}
test :: Arrow a => a b Bool -> a b (Either b b)
test f =
    (f &&& arr id) >>>
    arr (\(b,x) -> if b then Left x else Right x)

