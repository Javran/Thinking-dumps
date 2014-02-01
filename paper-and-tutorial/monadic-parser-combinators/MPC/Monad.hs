{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- let's try to make our own version of monad
module MPC.Monad
where

-- shadowing conflicting definitions in Prelude
import Prelude hiding (Maybe, Just, Nothing, maybe)
import Control.Monad

-- the exception monad

data Maybe a = Just a | Nothing
    deriving (Show, Eq)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ convert (Just x) = convert x
maybe fail _ _ = fail

instance Monad Maybe where
    return = Just
    m >>= f = maybe Nothing f m

-- the non-determinism monad

data Lst a = Empty | Lst a (Lst a)
    deriving (Show, Eq)

instance Functor Lst where
   fmap f xs = case xs of
        Empty -> Empty
        Lst x xrest -> Lst (f x) (fmap f xrest)

concatLst :: Lst (Lst a) -> Lst a
concatLst Empty = Empty
concatLst (Lst x xs) =
    x `plusLst` concatLst xs

plusLst :: Lst a -> Lst a -> Lst a
Empty `plusLst` ys = ys
(Lst x xs) `plusLst` ys = Lst x (xs `plusLst` ys)

instance Monad Lst where
    return x = Lst x Empty
    m >>= f = concatLst $ fmap f m

instance MonadPlus Lst where
    mzero = Empty
    mplus = plusLst

fromList :: [a] -> Lst a
fromList xs = foldr Lst Empty xs

toList :: Lst a -> [a]
toList Empty = []
toList (Lst x xs) = x : toList xs

-- the state monad
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    m >>= f = State $ \s1 ->
        let (a1,s2) = runState m s1
        in runState (f a1) s2

class Monad m => StateMonad m s where
    -- apply a function on the current state
    update :: (s -> s) -> m s
    -- replace the previous state with a new one
    --   returns the old state
    set    :: s -> m s
    -- return the current state
    fetch  :: m s

    set newS = update $ const newS
    fetch    = update id

instance StateMonad (State s) s where
    update f = State $ \s -> (s, f s)
