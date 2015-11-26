{-# LANGUAGE BangPatterns #-}
module StreamLimit where

import Control.Monad.Par
import Control.DeepSeq
import Debug.Trace

-- thanks to: http://stackoverflow.com/a/24780851/315302

data IList a
  = Nil
  | Cons a (IVar (IList a))
    -- the exercise asks we to add this alternative
    -- and fill in blanks
    -- INVARIANT: Fork _ (Cons _ _) is the only possible construction
    -- meaning that Fork _ Nil and Fork _ (Fork _ _) is invalid
    -- I understand by modeling Fork in this way we can use some code
    -- but there is another way: `Fork (Par ()) a (IVar (IList a))`
    -- this prevents invalid constructions at the cost of a more complex type
  | Fork (Par ()) (IList a)

type Stream a = IVar (IList a)

-- @IList a@ needs to be NFData if @a@ is one instance of it
-- note that forcing @tl@ in @Cons v tl@ will only cause the
-- reference @IVar@ to be valued to NF, to get the value out of it
-- one still needs @get@
instance NFData a => NFData (IList a) where
    rnf Nil = ()
    rnf (Cons v tl) = v `seq` tl `seq` ()
    rnf (Fork m n) = m `seq` n `seq` ()

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

streamFromListWithLimit :: NFData a => Int -> [a] -> Par (Stream a)
streamFromListWithLimit lim xs = do
    var <- new
    let loop [] r _ = put r Nil
        loop (y:ys) r remaining = do
            tl <- new
            -- put r (Cons y tl)
            if remaining == 0
              then put r (Fork (trace "restarting producer" $loop ys tl lim) (Cons y tl))
              else do
                  put r (Cons y tl)
                  loop ys tl (remaining-1)
    -- a fork is necessary to allow the task to be dispatched
    -- in parallel
    -- (a consumer forks its task)
    fork (loop xs var lim)
    return var

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold func !seed = consumeStream onNil onCons
  where
    onNil = return seed
    onCons v = streamFold func (seed `func` v)

-- to use this function, one should provide two callbacks: onNil and onCons
-- the case of Fork is handled automatically so that the user of this
-- function don't need to take care of them
consumeStream :: Par b
              -> (a -> Stream a -> Par b)
              -> Stream a
              -> Par b
consumeStream onNil onCons stream = do
    st <- get stream
    case st of
        Nil -> onNil
        Cons v tl -> onCons v tl
        Fork m (Cons v tl) -> fork m >> onCons v tl
        Fork _ _ -> error "invalid stream construction"

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
    loop instrm outstrm = consumeStream onNil onCons instrm
      where
        onNil = put outstrm Nil
        onCons h tl = do
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
            s <- streamFromListWithLimit 20000 xs
            s1 <- streamMap showD s
            s2 <- streamMap (readD :: String -> Integer) s1
            streamFold (+) 0 s2
        xs = [1 :: Int .. 100000]
    print (runPar result)
