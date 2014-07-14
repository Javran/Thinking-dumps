module Problem25 where

import Control.Applicative
import Control.Monad.Random

import Problem23 (rndSelectNoRep)

rndPerm :: MonadRandom m => [a] -> m [a]
rndPerm = rndSelectNoRep <*> length

main :: IO ()
main = rndPerm "abcdefg" >>= print
