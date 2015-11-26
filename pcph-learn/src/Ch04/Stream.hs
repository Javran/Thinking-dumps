{-# LANGUAGE BangPatterns #-}
-- ghc -O2 -rtsopts -threaded -fforce-recomp -eventlog -main-is Stream Stream.hs
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
    -- (a consumer forks its task)
    fork (loop xs var)
    return var

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold func !seed r = do
    stream <- get r
    case stream of
        Nil -> return seed
        Cons v tl -> streamFold func (seed `func` v) tl

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap f i = do
    o <- new
    -- a mapper is both a consumer and a producer
    -- so it has both:
    -- * case-expression to consume incoming data stream
    -- * task-forking for producing data stream
    fork (loop i o)
    return o
  where
    loop instrm outstrm = do
        ilst <- get instrm
        case ilst of
            Nil -> put outstrm Nil
            (Cons h tl) -> do
                newtl <- new
                put outstrm (Cons (f h) newtl)
                loop tl newtl

{-
  Test is performed by using (read . show) on a list of integers and calculate the sum
  looks like the number passed as "-N{num}" has a large impact on productivity:
  if we use BangPatterns on Cons,
  *  -N4 works the best while other numbers result in only ~50% productivity.
  if we don't use it, -N5 works the best (but not very good)
-}

main :: IO ()
main = do
    let -- for debugging
        showD = show :: Int -> String
        readD = read :: String -> Integer
        result = do
            s <- streamFromList xs
            s1 <- streamMap showD s
            s2 <- streamMap (readD :: String -> Integer) s1
            streamFold (+) 0 s2
        xs = [1 :: Int .. 100000]
    print (runPar result)
