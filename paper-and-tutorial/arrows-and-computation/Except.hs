module Except where

import qualified Control.Category as Cat
import Control.Arrow
import Common
import Data.Either

newtype Except a b c = E (a b (Either String c))

arrE :: Arrow a => (i -> o) -> Except a i o
arrE f = E (arr (Right . f))

collapse :: Either a (Either a b) -> Either a b
collapse e = case e of
    Left m -> Left m
    Right (Left m) -> Left m
    Right (Right v) -> Right v

compE :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr a c
compE (E f) (E g) = E $ f >>> right g >>> arr collapse

instance ArrowChoice a => Cat.Category (Except a) where
    id = arrE id
    g . f = compE f g

instance ArrowChoice a => Arrow (Except a) where
    arr = arrE
    first (E f) = E $ first f >>> arr (either (Left . fst) Right . distr)

{-

functor axiom: first (f >>> g) = first f >>> first g

distribution:

first (left f) >>> arr distr = arr distr >>> left (first f)

note that:
- first mirror >>> distr :: (Either a b, c) -> Either (b, c) (a, c)
- distr >>> mirror :: (Either a b, c) -> Either (b, c) (a, c)

we can conclude that: first mirror >>> distr = distr >>> mirror

and therefore the following is true:
first (right f) >>> arr distr = arr distr >>> right (first f)

LHS:
first (right f) >>> arr distr
=> first (arr mirror) >>> first (left f) >>> first (arr mirror) >>> arr distr
=> first mirror >>> first (left f) >>> first mirror >>> distr
=> first mirror >>> first (left f) >>> distr >>> mirror
=> first mirror >>> distr >>> left (first f) >>> mirror
=> distr >>> mirror >>> left (first f) >>> mirror
=> distr >>> right (first f)
=> arr distr >>> right (first f) = RHS

(TODO) getting bored, too much time on this, skipping it for now.

-}

rhs :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr (a,d) (c,d)
rhs (E f) (E g) = E $ first f >>> arr (distr >>> left fst)
                  >>> right (first g >>> arr (distr >>> left fst)) >>> arr collapse

lhs :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr (a,d) (c,d)
lhs (E f) (E g) = E $ first (f >>> right g >>> arr collapse) >>> arr (distr >>> left fst)
