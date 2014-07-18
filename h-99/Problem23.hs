module Problem23 where

import Control.Monad
import Control.Monad.Random

import Problem20 (removeAt)

rndOne :: (MonadRandom m) => [a] -> m (a,[a])
rndOne xs = liftM (`removeAt` xs) $ getRandomR (1,length xs)

-- select elements randomly with replacement
rndSelectRep :: (MonadRandom m) => [a] -> Int -> m [a]
rndSelectRep xs n = replicateM n (liftM fst $ rndOne xs)

-- select elements randomly without replacement
rndSelectNoRep :: (MonadRandom m) => [a] -> Int -> m [a]
rndSelectNoRep xs n
    | n < 0 || n > l = error "Impossible"
    | n == 0 = return []
    | otherwise = do (x,ys) <- rndOne xs
                     rest <- rndSelectNoRep ys (n-1)
                     return $ x : rest
    where
        l = length xs

main :: IO ()
main = do
    rndSelectRep   "abcdefg" 7 >>= print
    rndSelectNoRep "abcdefg" 7 >>= print
