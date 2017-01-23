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
