module Robot
    ( robotName
    , mkRobot
    , resetName)
where

import System.Random
import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.IORef

type Factory = IORef StdGen

-- | produce a robot name
robotName :: Factory -> IO String
robotName = readIORef >=> evalRandT robotNameAp

-- | randomly generate a robot name
robotNameAp :: (MonadRandom m, Applicative m) => m String
robotNameAp = (++) <$> replicateM 2 mkChar
                   <*> replicateM 3 mkNum
    where
        mkChar = getRandomR ('A','Z')
        mkNum  = getRandomR ('0','9')

-- | make a new robot
mkRobot :: IO Factory
mkRobot = newStdGen >>= newIORef

-- | reset factory
resetName :: Factory -> IO ()
resetName ref = newStdGen >>= writeIORef ref

