{-# LANGUAGE GADTs #-}
module StateEff where

import Free hiding (FFState)

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
