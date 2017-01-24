{-# LANGUAGE GADTs #-}
module StateEff where

import Free hiding (FFState, runFFState)

-- the definition of StateEff alone does not do anything,
-- but it still make (FFree (StateEff s)) a monad.
-- the actual implementation is moved to the interpreter.
data StateEff s x where
    Get :: StateEff s s
    Put :: s -> StateEff s ()

type FFState s = FFree (StateEff s)

getEff :: FFState s s
getEff = etaF Get

putEff :: s -> FFState s ()
putEff = etaF . Put

{-
redoing modFState and testFState.
this time we don't even need to have a instance of Functor.
-}

modFFState :: (s -> s) -> FFState s s
modFFState f = do
    x <- getEff
    putEff (f x)
    pure x

testFFState :: FFState Int Int
testFFState = do
    putEff 10
    modFFState (+ 20)

{-
but actually this is a lie: we have to give an implementation somewhere,
and for freer monad this relies on the "runXXX" function.

so if I understand it correctly: Freer monad (by the approach described by this file)
let us write a description of what we are doing without the requirement of actually
implementing it. and the task of implementation is then moved to the interpreter
of the monad.
-}
runFFState :: FFState s a -> s -> (a,s)
runFFState m s = case m of
    FPure v -> (v,s)
    FImpure gx q -> case gx of
        Get -> runFFState (q s) s
        Put s' -> runFFState (q ()) s'
