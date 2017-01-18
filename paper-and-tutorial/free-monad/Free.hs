{-# LANGUAGE GADTs, LambdaCase #-}
module Free where

import Data.Function

-- http://okmij.org/ftp/Computation/free-monad.html

data Free f a where
    Pure   :: a -> Free f a
    Impure :: f (Free f a) -> Free f a

-- wrapping arbitrary functor into Free:
{-
- we begin with:

eta :: Functor f => f a -> Free f a
eta f = _ :: Free f a

- "Pure" is not possible, as we lack a value of type "a", so "Impure" instead:

eta :: Functor f => f a -> Free f a
eta f = Impure (_ :: f (Free f a))

- we already have "f :: f a", now if we can just find something that
  turns "a" into "Free f a", then we are done. and "Pure" is exactly what we are looking for:

eta :: Functor f => f a -> Free f a
eta f = Impure (Pure <$> f)

- and the final version is point-free:
-}
eta :: Functor f => f a -> Free f a
eta = Impure . fmap Pure

instance Functor f => Functor (Free f) where
    fmap f = fix $ \ff -> \case
        Pure x -> Pure (f x)
        Impure m -> Impure (fmap ff m)

instance Functor f => Applicative (Free f) where
    pure = Pure
    ff <*> m = case ff of
        Pure v -> fmap v m
        Impure f -> Impure (fmap (<*> m) f)
