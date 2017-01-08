module Except where

import qualified Control.Category as Cat
import Control.Arrow
import Common
import Data.Either

newtype Except a b c = E (a b (Either String c))

arrE :: Arrow a => (i -> o) -> Except a i o
arrE f = E (arr (Right . f))

compE :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr a c
compE (E f) (E g) = E $ f >>> right g >>> arr collapse
  where
    collapse e = case e of
        Left m -> Left m
        Right (Left m) -> Left m
        Right (Right v) -> Right v

instance ArrowChoice a => Cat.Category (Except a) where
    id = arrE id
    g . f = compE f g

instance ArrowChoice a => Arrow (Except a) where
    arr = arrE
    first (E f) = E $ first f >>> arr (either (Left . fst) Right . distr)

{-

functor axiom: first (f >>> g) = first f >>> first g

-}

rhs :: ArrowChoice arr => Except arr a b -> Except arr b c -> Except arr (a,d) (c,d)
rhs (E f) (E g) = E $ (first f >>> arr h) >>> right (first g >>> arr h) >>> arr collapse
  where
    h = either (Left . fst) Right . distr
    collapse e = case e of
        Left m -> Left m
        Right (Left m) -> Left m
        Right (Right v) -> Right v
