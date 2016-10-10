{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module CBAny where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

-- the arrow (higher-order abstract syntax) type
type Arr repr a b = repr a -> repr b

-- without environment, so that'll be embedded in metalanguage
class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    sub :: repr Int -> repr Int -> repr Int
    app :: repr (Arr repr a b) -> repr a -> repr b

-- "lam" is seperated for the reason that this is the only difference
-- between 3 evaluation strategies: call-by-value, call-by-name, call-by-need
-- TODO: question: why the difference is in "lam"? I expected it to have something
-- to do with "app".
class SymLam repr where
    lam :: (repr a -> repr b) -> repr (Arr repr a b)

newtype S l m a = S { unS :: m a }
  deriving (Monad, Applicative, Functor, MonadIO)

instance MonadIO m => Symantics (S l m) where
    int = pure
    add x y = do
        a <- x
        b <- y
        liftIO $ putStrLn "Adding"
        pure (a+b)
    sub x y = do
        a <- x
        b <- y
        liftIO $ putStrLn "Subtracting"
        pure (a-b)
    -- NOTE: I was wondering why "x" has to be evaluated
    -- earlier than y, but that's actually the right thing to do
    -- despite of evaluation strategy
    app x y = x >>= ($ y)
