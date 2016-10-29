{-# LANGUAGE
    KindSignatures
  , FlexibleContexts
  , ScopedTypeVariables
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , DataKinds
  , GADTs
  #-}

module DependentZipWith where

data Nat = Zero | Succ Nat

{-
   n: number of argument lists we are going to zip
   arrows: the zipping function

   e.g. Listify 0 a = [a]
        Listify 3 (a -> b -> c -> d) = [a] -> [b] -> [c] -> [d]
        Listify 2 (a -> b -> c -> d) = [a] -> [b] -> [c -> d]
 -}
type family Listify (n :: Nat) arrows where
    Listify 'Zero a = [a]
    Listify ('Succ n) (a -> b) = [a] -> Listify n b

-- relying on GADTs to provide us evidence on destruction
data NumArgs :: Nat -> * -> * where
    NAZero :: NumArgs 'Zero a
    NASucc :: NumArgs n b -> NumArgs ('Succ n) (a -> b)

{-# ANN listApply "HLint: ignore Avoid lambda" #-}
listApply :: NumArgs n a -> [a] -> Listify n a
listApply NAZero fs = fs
listApply (NASucc na) fs = \args -> listApply na (zipWith ($) fs args)

{-
  now we can imagine the zipWith' we are building have the following type:

  zipWith' :: f -> Listify <?> f

  which eventually expands to (depending on the <?>):

  zipWith' :: f -> ( <0> -> <1> -> ... -> <n> )

  and we now focus on how to get the "<?>" part inferred.
-}

{-
   a type function for counting arity of the "zipping function"
   the rule is simple:
   - whenever the pattern is "(->) a b" (top-level arrow)", the counter bumps
     and "b" is recursively counted
   - otherwise we think it's a non-function type, and stop.

   we lose some flexibility here: "a -> b -> c" is always having 2 arrows
   despite it can be viewed as "a -> d" where "d ~ (b -> c)"
-}
type family CountArgs (f :: *) :: Nat where
    CountArgs (a -> b) = 'Succ (CountArgs b)
    CountArgs result = 'Zero

{-
  after counting how many arrows we have, we want to
  be able to build up evidence that can be consumed by "listApply"
  to complete our implementation, this step is done by typeclass CNumArgs:
  with type inference done, a value of NASucc (NASucc (... NAZero)) is built
-}
class CNumArgs (numArgs :: Nat) (arrows :: *) where
    getNA :: NumArgs numArgs arrows

{-
  the following instances won't overlap,
  as their first argument patterns cannot be unified
-}

instance CNumArgs 'Zero a where
    getNA = NAZero

instance CNumArgs n b => CNumArgs ('Succ n) (a -> b) where
    getNA = NASucc getNA

zipWith' :: forall f. CNumArgs (CountArgs f) f =>
           f -> Listify (CountArgs f) f
zipWith' f = listApply (getNA :: NumArgs (CountArgs f) f) (repeat f)
