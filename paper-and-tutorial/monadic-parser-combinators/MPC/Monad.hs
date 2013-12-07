-- let's try to make our own version of monad
module MPC.Monad
where

import Control.Monad

-- the exception monad

data Maybe1 a = Just1 a | Nothing1
    deriving Show

maybe1 :: b -> (a -> b) -> Maybe1 a -> b
maybe1 _ convert (Just1 x) = convert x
maybe1 fail _ _ = fail

instance Monad Maybe1 where
    return = Just1
    m >>= f = maybe1 Nothing1 f m
