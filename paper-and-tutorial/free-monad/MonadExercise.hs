{-# LANGUAGE GADTs #-}
module MonadExercise where

-- just try to write some standard monads for fun.

import Free (FFree(..), etaF)
import Data.Monoid

data WriterEff w a where
    Writer :: (a,w) -> WriterEff w a
    Tell :: w -> WriterEff w ()

type FFWriter w = FFree (WriterEff w)

writerEff :: (a,w) -> FFWriter w a
writerEff = etaF . Writer

tellEff :: w -> FFWriter w ()
tellEff = etaF . Tell

runWriter :: Monoid w => FFWriter w a -> (a,w)
runWriter (FPure v) = (v, mempty)
runWriter (FImpure gx q) = case gx of
    Writer (a,w) -> let (a',w') = runWriter (q a)
                    in (a',w `mappend` w')
    Tell w -> let (a',w') = runWriter (q ())
              in (a',w `mappend` w')

writeNats :: Int -> FFWriter [Int] ()
writeNats from = writerEff ((),[from]) >> writeNats (succ from)
