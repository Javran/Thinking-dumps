{-# LANGUAGE
    FlexibleInstances
  , KindSignatures
  , DataKinds
  , GeneralizedNewtypeDeriving
  #-}
module CBAny where

import Data.IORef
import Control.Monad
import Control.Monad.Trans

{-# ANN module "HLint: ignore Eta reduce" #-}

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

{-
  NOTE:

  I was wondering why the difference is in "lam" rather than "app",
  since different evaluation strategies actually specifies when
  an argument is evaluated upon function application.

  recall that:
  - call-by-value always forces the argument fully
  - call-by-name subsitutes the definition for the variable
  - call-by-need creates a reference, but keeps the definition body
    as a thunk for future evaluation and sharing

  but here is a key observation: no matter what strategy we use,
  for a function application, that function must be evaluated first (at least to WHNF
  so that we know this is a function in the first place),
  this explains one part of the following implementation:

  app x y = x >>= ($ y)

  which can be desugared to:

  app x y = x >>= \f -> f y

  from here we know that "x" must be evaluated earlier than "y",
  and also in this implementation, nothing forces "y",
  so we have the full freedom of deciding how to deal with "y"
  in our implementation of "lam" thanks to Haskell laziness.

  so here I want to have a quick conclusion saying:
  - how to implement different strategies depends on what metalanguage
    we are using: if we are using a strict language, by the time when
    we get to the lambda expression, the argument would have been evaluated
    so it would work better if we choose to share "lam" but have "app" implemented
    separately for different evaluation strategies.
-}
class SymLam repr where
    lam :: (repr a -> repr b) -> repr (Arr repr a b)

-- just trying to use promoted type for a more accurate type
data EStrategy = CBValue | CBName | CBNeed

newtype S (l :: EStrategy) m a = S { unS :: m a }
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
    app x y = x >>= ($ y)

instance Monad m => SymLam (S 'CBName m) where
    lam f = pure f

runName :: S 'CBName m a -> m a
runName = unS

instance Monad m => SymLam (S 'CBValue m) where
    lam f = pure (\x -> x >>= f . pure)

runValue :: S 'CBValue m a -> m a
runValue = unS

share :: MonadIO m => m a -> m (m a)
share m = do
    -- store unevaluated thunk "m" to "r"
    r <- liftIO $ newIORef (False, m)
    pure (do
        -- read "r" and see if the result is available
        (f,m') <- liftIO $ readIORef r
        if f
          -- when the result is available, we use that
          -- instead of doing "m" again
          then m'
          else do
            -- otherwise we have to evaluate "m"
            v <- m
            -- and store its value as a replacement for future.
            liftIO $ writeIORef r (True, pure v)
            pure v)
