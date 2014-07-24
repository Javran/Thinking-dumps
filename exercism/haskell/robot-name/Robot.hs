module Robot
    ( robotName
    , mkRobot
    , resetName)
where

import System.Random
import Control.Applicative
import Control.Monad.Random
import Data.IORef

type RHandler = IORef String

-- | produce a robot name
robotName :: RHandler -> IO String
robotName = readIORef

-- | randomly generate a robot name
robotNameAp :: (MonadRandom m, Applicative m) => m String
robotNameAp = mapM getRandomR (rep' ch ++ rep' nu)
    where
        rep' = uncurry replicate
        ch = (2,('A','Z'))
        nu = (3,('0','9'))

-- | make a new robot
mkRobot :: IO RHandler
mkRobot = genNewName >>= newIORef

-- | reset factory
resetName :: RHandler -> IO ()
resetName ref = genNewName >>= writeIORef ref

-- | generate a new name
genNewName :: IO String
genNewName = newStdGen >>= evalRandT robotNameAp
