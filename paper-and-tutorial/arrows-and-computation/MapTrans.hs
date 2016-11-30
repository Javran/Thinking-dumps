module MapTrans where

import qualified Control.Category as Cat
import Control.Arrow

{-# ANN module "HLint: ignore Redundant bracket" #-}

newtype MapTrans s i o = MT { runMT :: (s -> i) -> (s -> o) }

arrMT :: (i -> o) -> MapTrans s i o
arrMT f = MT $ \k -> f . k

compMT :: MapTrans s a b -> MapTrans s b c -> MapTrans s a c
compMT (MT f) (MT g) = MT (g . f)

instance Cat.Category (MapTrans s) where
  id = arrMT id
  g . f = compMT f g

instance Arrow (MapTrans a) where
    arr = arrMT
    first (MT f) = MT (zipMap . first f . unzipMap)

{-
there is a subtle issue with "zipMap" and "unzipMap":
while "zipMap" dispatches its input to two functions
"unzipMap" executes the function twice: first time for extracting "fst" part of it
and the second time for "snd" part. if the function body is "lazy" enough
then it's fine, but if somehow to evaluate the function requires evaluating both "fst" and "snd"
part, then we might have some problem regarding efficiency.
-}
zipMap :: (s -> a, s -> b) -> (s -> (a,b))
zipMap (f,g) s = (f s, g s)

unzipMap :: (s -> (a,b)) -> (s -> a, s -> b)
unzipMap h = (fst . h, snd . h)
