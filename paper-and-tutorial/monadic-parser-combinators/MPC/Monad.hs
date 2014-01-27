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
fromList [] = Empty
fromList (x:xs) = Lst x (fromList xs)

toList :: Lst a -> [a]
toList Empty = []
toList (Lst x xs) = x : toList xs
