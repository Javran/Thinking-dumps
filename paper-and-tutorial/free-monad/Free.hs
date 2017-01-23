{-# LANGUAGE GADTs, LambdaCase, ScopedTypeVariables #-}
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

data Lan g a where
    Lan :: g x -> (x -> a) -> Lan g a

instance Functor (Lan g) where
    -- note that: "gx" is passed around but never touched,
    -- and whatever "fab" is gets attached to the second part of "Lan"
    fmap fab (Lan gx fxa) = Lan gx (fab . fxa)

lan :: g a -> Lan g a
lan ga = Lan ga id

data FList a = FNil | FCons a (FList a) deriving Show

test :: Lan FList Int
test = fmap (* 10) (lan (FCons 1 (FCons 2 (FCons (3 :: Int) FNil))))

-- and to extract things out we still need to implement something like "fmap"
getFList :: Lan FList a -> FList a
getFList (Lan lstX fxa) = case lstX of
    FNil -> FNil
    FCons x xs -> FCons (fxa x) (getFList (Lan xs fxa))

-- Freer monad
data FFree g a where
    FPure :: a -> FFree g a
    FImpure :: g x -> (x -> FFree g a) -> FFree g a

{-

Recall the definition of Free and Lan:

data Free f a where
    Pure   :: a -> Free f a
    Impure :: f (Free f a) -> Free f a

data Lan g a where
    Lan :: g x -> (x -> a) -> Lan g a

and "Free (Lan g)" should be a freer monad, we plugin (f = Lan g) on "Free":

data Free f a where
    Pure   :: a -> Free f a

    Impure :: f (Free f a) -> Free f a
    ===> (f = Lan g)
    Impure :: Lan g (Free f a) -> Free f a
    ===> destruct "Lan"
    Impure :: g x -> (x -> Free f a) -> Free f a

and this is exactly "FFree"
-}

instance Functor (FFree g) where
    fmap f = fix $ \ff m ->
        case m of
            FPure x -> FPure (f x)
            FImpure gx q -> FImpure gx (ff . q)

instance Applicative (FFree g) where
    pure = FPure
    ma <*> mb = case ma of
        FPure f -> fmap f mb
        FImpure gx q -> FImpure gx ((<*> mb) . q)

instance Monad (FFree g) where
    m >>= g = case m of
        FPure f -> g f
        FImpure gx q -> FImpure gx ((>>= g) . q)

{-
no Functor constraint, any "g a" structure is turned into "FFree g a",
which is an instance of Monad
-}
etaF :: g a -> FFree g a
etaF ga = FImpure ga FPure

{-
the full version should be:

type FFState s a = FFree (State s) a

-}
type FFState s = FFree (State s)

runFFState :: FFState s a -> s -> (a,s)
runFFState m s = case m of
    FPure a -> (a,s)
    FImpure (State f) q ->
        let (x,s') = f s
            m' = q x
        in runFFState m' s'

-- TODO: definitional interpreters for effects
-- TODO: extensible: http://okmij.org/ftp/Haskell/extensible/
