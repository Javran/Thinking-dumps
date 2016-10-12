{-# LANGUAGE
    FlexibleInstances
  , NoMonomorphismRestriction
  , RankNTypes
  , KindSignatures
  , DataKinds
  , GeneralizedNewtypeDeriving
  #-}
module CBAny where

import Data.IORef
import Control.Monad.Trans

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use >=>" #-}
{-# ANN module "HLint: ignore Move brackets to avoid $" #-}

-- http://okmij.org/ftp/tagless-final/course/CBAny.hs

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

share' :: MonadIO m => m a -> m (m a)
share' m = do
    -- store the result as a Maybe value
    -- we don't have to store "m" because that value
    -- is already available to use through context (argument to "share'")
    r <- liftIO $ newIORef Nothing
    pure (do
        -- read "r" and see if the result is available
        th <- liftIO $ readIORef r
        case th of
            Just v ->
                -- no computation, just retrieving
                -- the result we have already stored
                pure v
            Nothing -> do
                -- force "m" and store the value for future use
                v <- m
                liftIO $ writeIORef r $ Just v
                pure v)

instance MonadIO m => SymLam (S 'CBNeed m) where
    -- it should also be fine to use "share'" in place of "share"
    lam f = pure (\x -> share x >>= f)

runLazy :: S 'CBNeed m a -> m a
runLazy = unS

let_ :: (Symantics repr, SymLam repr) => repr a -> (repr a -> repr b) -> repr b
let_ x y = lam y `app` x

t0 :: (Symantics repr, SymLam repr) => repr Int
t0 = (lam $ \x -> let_ (x `add` x)
                 $ \y -> y `add` y) `app` int 10
-- t0 = (\x -> let y = x + x in y + y) 10
{-
small steps:

note that the "(Adding)" and "(Subtracting)" mark always indicates the operation
going to be performed immediately after that line, I made a mistake in
t2 "call-by-name" evaluation strategy because of being unclear about this marking rule.

call-by-value:
(\x -> let y = x + x in y + y) 10
=> let y = 10+10 in y+y (Adding)
=> let y = 20 in y+y
=> 20+20 (Adding)
=> 40

call-by-name:
(\x -> let y = x + x in y + y) 10
=> let y = 10 + 10 in y+y
=> (10+10)+(10+10) (Adding)
=> 20+(10+10) (Adding)
=> 20+20 (Adding)
=> 40

call-by-need:
(\x -> let y = x + x in y + y) 10
=> let y = x+x in y+y [mem: x -> 10]
=> y+y [mem: x -> 10, y -> x+x]
=> y+y [mem: x -> 10, y -> 20] (Adding)
=> 20+20 (Adding)
=> 40
-}

t1 :: (Symantics repr, SymLam repr) => repr Int
t1 = (lam $ \x -> let_ (x `add` x)
                  $ \y -> lam $ \z ->
                  z `add` (z `add` (y `add` y)))
     `app` (int 10 `sub` int 5)
     `app` (int 20 `sub` int 10)
-- t1 = (\x -> let y = x+x in \z -> z+(z+(y+y))) (10-5) (20-10)
{-
small steps:

call-by-value:
(\x -> let y = x+x in \z -> z+(z+(y+y))) (10-5) (20-10)
=> (\x -> let y = x+x in \z -> z+(z+(y+y))) 5 (20-10) (Subtracting)
=> (let y = 5+5 in \z -> z+(z+(y+y))) (20-10)
=> (let y = 10 in \z -> z+(z+(y+y))) (20-10) (Adding)
=> (\z -> z+(z+(10+10))) (20-10) (Subtracting)
=> (\z -> z+(z+(10+10))) 10
=> 10+(10+(10+10)) (Adding)
=> 10+(10+20) (Adding)
=> 10+30 (Adding)
=> 40

call-by-name:
(\x -> let y = x+x in \z -> z+(z+(y+y))) (10-5) (20-10)
=> (let y = (10-5)+(10-5) in \z -> z+(z+(y+y))) (20-10)
=> (\z -> z+(z+( ((10-5)+(10-5))+((10-5)+(10-5)) ))) (20-10)
=> (20-10)+((20-10)+( ((10-5)+(10-5))+((10-5)+(10-5)) )) (Subtracting)
=> 10+((20-10)+( ((10-5)+(10-5))+((10-5)+(10-5)) )) (Subtracting)
=> 10+(10+( ((10-5)+(10-5))+((10-5)+(10-5)) )) (Subtracting)
=> 10+(10+( (5+(10-5))+((10-5)+(10-5)) )) (Subtracting)
=> 10+(10+( (5+5)+((10-5)+(10-5)) )) (Adding)
=> 10+(10+( 10+((10-5)+(10-5)) )) (Subtracting)
=> 10+(10+( 10+(5+(10-5)) )) (Subtracting)
=> 10+(10+( 10+(5+5) )) (Adding)
=> 10+(10+( 10+10 )) (Adding)
=> 10+(10+20) (Adding)
=> 10+30 (Adding)
=> 40

call-by-need:
(\x -> let y = x+x in \z -> z+(z+(y+y))) (10-5) (20-10)
=> (let y = x+x in \z -> z+(z+(y+y))) (20-10) [mem: x->10-5]
=> (\z -> z+(z+(y+y))) (20-10) [mem: x->10-5, y->x+x]
=> z+(z+(y+y)) [mem: x->10-5, y->x+x, z->20-10] (Subtracting)
=> z+(z+(y+y)) [mem: x->10-5, y->x+x, z->10] (Subtracting)
=> z+(z+(y+y)) [mem: x->5, y->x+x, z->10] (Adding)
=> z+(z+(y+y)) [mem: x->5, y->10, z->10] (Adding) (y+y)
=> z+(z+20) [mem: x->5, y->10, z->10] (Adding) (z+20)
=> z+30 [mem: x->5, y->10, z->10] (Adding) (z+30)
=> 40
-}

-- "_z" for indicating that this variable is not used at all.
t2 :: (Symantics repr, SymLam repr) => repr Int
t2 = (lam $ \_z -> lam $ \x -> let_ (x `add` x)
                      $ \y -> y `add` y)
     `app` (int 100 `sub` int 10)
     `app` (int 5 `add` int 5)
-- t2 = (\z -> \x -> let y = x+x in y+y) (100-10) (5+5)
{-
small steps:

call-by-value:
(\z -> \x -> let y = x+x in y+y) (100-10) (5+5) (Subtracting)
=> (\z -> \x -> let y = x+x in y+y) 90 (5+5)
=> (\x -> let y = x+x in y+y) (5+5) (Adding)
=> (\x -> let y = x+x in y+y) 0
=> let y = 0+0 in y+y (Adding)
=> 0+0 (Adding)
=> 0

call-by-name:
(\z -> \x -> let y = x+x in y+y) (100-10) (5+5)
=> (\x -> let y = x+x in y+y) (5+5)
=> let y = (5+5)+(5+5) in y+y
=> ( (5+5)+(5+5) ) + ( (5+5)+(5+5) ) (Adding)
=> ( 0+(5+5) ) + ( (5+5)+(5+5) ) (Adding)
=> ( 0+0 ) + ( (5+5)+(5+5) ) (Adding)
=> 0 + ( (5+5)+(5+5) ) (Adding)
=> 0 + ( 0+(5+5) ) (Adding)
=> 0 + ( 0+0 ) (Adding)
=> 0 + 0 (Adding)
=> 0

call-by-need:
(\z -> \x -> let y = x+x in y+y) (100-10) (5+5)
=> (\x -> let y = x+x in y+y) (5+5) [mem: z->5+5]
=> let y = x+x in y+y [mem: z->5+5, x->5+5]
=> y+y [mem: z->5+5, x->5+5, y->x+x] (Adding)
=> y+y [mem: z->5+5, x->0, y->x+x] (Adding)
=> y+y [mem: z->5+5, x->0, y->0] (Adding)
=> 0

-}

-- existential types are good for delaying instance selection, see
-- the use of "Dyn" for an example.
newtype Dyn a = Dyn (forall repr. (Symantics repr, SymLam repr) => repr a)

testExpr :: Dyn Int -> IO ()
testExpr expr = do
    let (Dyn e) = expr
        runWith tag f = do
            putStrLn (unwords ["using", tag, "strategy:"])
            v <- f e
            putStrLn ("result value: " ++ show v)

    runWith "call-by-value" runValue
    runWith "call-by-name" runName
    runWith "call-by-need" runLazy

main :: IO ()
main = mapM_ testExpr [Dyn t0, Dyn t1, Dyn t2]

{-
  it's tempting to write it like the following:

> main = mapM_ (testExpr . Dyn) [t0, t1, t2]

  but this code won't typecheck, the main reason is that we are trying to
  unify two "forall" types, which won't work, the compiler complains:

  "cannot instantiate unifciation with a type involving foralls"

-}
