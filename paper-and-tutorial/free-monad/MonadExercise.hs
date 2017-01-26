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

{-
this implementation builds up the Monoid in right-associated way:

- tell a >> tell b >> tell c

===> (a <> ?)
===> (a <> (b <> ?))
===> (a <> (b <> c))

but monoid property ensures that this shall be the same as ((a <> b) <> c)
despite that the performance might differ.
-}
runWriter :: Monoid w => FFWriter w a -> (a,w)
runWriter (FPure v) = (v, mempty)
runWriter (FImpure gx q) = case gx of
    Writer (a,w) -> let (a',w') = runWriter (q a)
                    in (a',w <> w')
    Tell w -> let (a',w') = runWriter (q ())
              in (a',w <> w')

{-
same as "runWriter", but this time it associates to left:

- tell a >> tell b >> tell c

===> mempty
===> mempty <> a (= a)
===> a
===> a <> b
===> (a <> b) <> c

so now we have seen some advantages of Freer monad: we can have multiple
interpreters of the same monad and get different ways of computing things

-}
runWriterL :: Monoid w => FFWriter w a -> (a,w)
runWriterL = runWriterL' mempty
  where
    runWriterL' acc (FPure v) = (v, acc)
    runWriterL' acc (FImpure gx q) = case gx of
        Writer (a,w) -> runWriterL' (acc <> w) (q a)
        Tell w -> runWriterL' (acc <> w) (q ())

writeNats :: Int -> FFWriter [Int] ()
writeNats from = writerEff ((),[from]) >> writeNats (succ from)

{-

could be related, turns out list monad cannot be implemented this way:

http://stackoverflow.com/q/14641864/315302

note that Freer monads are just Free monads with left Kan-extension so that we get Functor
for free. if list monad cannot be implemented in terms of Free monad, Freer monad won't work either.

-}
