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
    -- TODO: not sure if this is correct,
    -- especially about the use of "const"
    first (MT f) = MT $ \k a -> let (b,d) = k a in (f (const b) a,d)
