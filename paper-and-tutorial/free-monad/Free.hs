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

instance Functor f => Monad (Free f) where
    ff >>= m = case ff of
        Pure v -> m v
        Impure f -> Impure (fmap (>>= m) f)

newtype State s a = State { unState :: s -> (a,s) }

get :: State s s
get = State $ \s -> (s,s)

{-# ANN put "HLint: ignore Use const" #-}
put :: s -> State s ()
put s = State $ \_ -> ((), s)

type FState s = Free (State s)

instance Functor (State s) where
    fmap f (State m) = State $ \s -> let (a,s') = m s in (f a,s')

getF :: FState s s
getF = eta get

putF :: s -> FState s ()
putF = eta . put

-- the actual work of `bind` is actually implemented by runFState
runFState :: FState s a -> s -> (a,s)
runFState (Pure x) s = (x,s)
runFState (Impure m) s = runFState m' s'
  where
    (State f) = m
    (m',s') = f s

-- modify old state, using a function, and return the old one
modFState :: (s -> s) -> FState s s
modFState f = do
    x <- getF
    putF (f x)
    pure x

testFState :: FState Int Int
testFState = do
    putF 10
    modFState (+ 20)
{-
> runFState testFState undefined
(10,30)
-}

joinFState :: State s (State s a) -> State s a
joinFState (State f) = State $ \s -> let (State m,s') = f s in m s'
