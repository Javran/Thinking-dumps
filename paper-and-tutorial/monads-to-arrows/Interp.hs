module Interp where

import Control.Arrow
import Data.Maybe

data Exp
  = Var String
  | Add Exp Exp
  | If Exp Exp Exp

data Val = VNum Int | VBl Bool

type Env = [(String,Val)]

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

eval :: (Arrow a, ArrowChoice a) => Exp -> a Env Val
eval (Var s) = arr (fromJust . lookup s)
eval (Add e1 e2) = liftA2 add (eval e1) (eval e2)
  where
    add (VNum x) (VNum y) = VNum (x+y)
    add _ _ = error "expecting numbers"
eval (If eIf eThen eElse) =
    (eval eIf &&& arr id) >>>
    arr (\(VBl b,env) -> if b then Left env else Right env) >>>
    (eval eThen ||| eval eElse)

-- (->) is an instance of Arrow, which means the following code does typecheck
eval' :: Exp -> Env -> Val
eval' = eval

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
