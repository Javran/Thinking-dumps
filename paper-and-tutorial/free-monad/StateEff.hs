{-# LANGUAGE GADTs #-}
module StateEff where

import Free

-- the definition of StateEff alone does not do anything,
-- but it still make (FFree (StateEff s)) a monad.
-- the actual implementation is moved to the interpreter.
data StateEff s x where
    Get :: StateEff s s
    Put :: s -> StateEff s ()

getEff :: FFree (StateEff s) s
getEff = etaF Get

putEff :: s -> FFree (StateEff s) ()
putEff = etaF . Put

{-
redoing modFState and testFState.
this time we don't even need to have a instance of Functor.
-}

modFFState :: (s -> s) -> FFree (StateEff s) s
modFFState f = do
    x <- getEff
    putEff (f x)
    pure x

testFFState :: FFree (StateEff Int) Int
testFFState = do
    putEff 10
    modFFState (+ 20)
