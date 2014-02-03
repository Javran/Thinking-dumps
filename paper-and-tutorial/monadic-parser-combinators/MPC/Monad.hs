{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- let's try to make our own version of monad
module MPC.Monad
where

-- shadowing conflicting definitions in Prelude
import Prelude hiding (foldr, Maybe, Just, Nothing, maybe)
import Control.Monad
import Data.Foldable hiding (toList)
import Data.Monoid

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

instance Foldable Lst where
    foldMap _ Empty = mempty
    foldMap f (Lst a b) = f a `mappend` foldMap f b

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
fromList = foldr Lst Empty

-- try Foldable
toList :: Lst a -> [a]
toList = foldr (:) []
-- toList Empty = []
-- toList (Lst x xs) = x : toList xs

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

newtype StateM m s a = StateM
    { runStateM :: s -> m (a,s) }

instance Monad m => Monad (StateM m s) where
    return x = StateM $ \s -> return (x,s)
    m >>= f = StateM $ \s1 ->
        let m1 = runStateM m s1
        in m1 >>= \(a1,s2) -> runStateM (f a1) s2

instance Monad m => StateMonad (StateM m s) s where
    update f = StateM $ \s -> return (s, f s)
