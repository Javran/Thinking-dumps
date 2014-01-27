-- let's try to make our own version of monad
module MPC.Monad
where

-- shadowing conflicting definitions in Prelude
import qualified Prelude as P
import qualified Control.Monad as M

-- the exception monad

data Maybe a = Just a | Nothing
    deriving (P.Show, P.Eq)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ convert (Just x) = convert x
maybe fail _ _ = fail

instance M.Monad Maybe where
    return = Just
    m >>= f = maybe Nothing f m
