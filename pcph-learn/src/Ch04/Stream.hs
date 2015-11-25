{-# LANGUAGE BangPatterns #-}
module Stream where

import Control.Monad.Par
import Control.DeepSeq

data IList a
  = Nil
  | Cons a (IVar (IList a))

type Stream a = IVar (IList a)

-- @IList a@ needs to be NFData if @a@ is one instance of it
-- note that forcing @tl@ in @Cons v tl@ will only cause the
-- reference @IVar@ to be valued to NF, to get the value out of it
-- one still needs @get@
instance NFData a => NFData (IList a) where
    rnf Nil = ()
    rnf (Cons v tl) = v `seq` tl `seq` ()

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
    var <- new
    let -- traverse the list and feed values to the stream
        loop [] r = put r Nil
        loop (y:ys) r = do
            tl <- new
            put r (Cons y tl)
            loop ys tl
    -- a fork is necessary to allow the task to be dispatched
    -- in parallel
    fork (loop xs var)
    return var

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold func !seed r = do
    stream <- get r
    case stream of
        Nil -> return seed
        Cons v tl -> streamFold func (seed `func` v) tl

main :: IO ()
main = do
    let result :: Par Int
        result = do
                  s <- streamFromList [1 :: Int ..100] :: Par (Stream Int)
                  streamFold (+) 1 s
    print (runPar result)
