{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Hom where

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad.State

-- homogeneous functions

{-
  "BalTree" is a collection of 2^n elements:

  - "Zero _" contains 1 = 2^0 element
  - "Succ (Zero (Pair _ _))" contains 2 = 2^1 elements

  note that the constructors form a string, which encodes the depth
  of this tree.
-}

data BalTree a = Zero a | Succ (BalTree (Pair a)) deriving Show

-- make a tree with given depth with elements given in the list
-- note that callers are responsible to produce sufficient elements
-- for creating the complete tree
makeTree :: Int -> [a] -> BalTree a
makeTree d = evalState (mkTree d)
  where
    next = state (\xs -> case xs of
                      [] -> error "source exhausted"
                      (y:ys) -> (y,ys))
    mkTree 0 = Zero <$> next
    mkTree dep = mergeTree <$> mkTree d' <*> mkTree d'
      where d' = dep-1

    -- use only for mering trees of the same depth
    mergeTree :: BalTree a -> BalTree a -> BalTree a
    mergeTree (Zero x) (Zero y) = Succ (Zero (x,y))
    mergeTree (Succ a) (Succ b) = Succ (mergeTree a b)
    mergeTree _ _ = error "merging two trees that have different depth"

type Pair a = (a,a)

{-
  seems we can expand it indefinitely:

  Hom a b
  <=> (a -> b) :&: Hom (Pair a) (Pair b)
  <=> (a -> b) :&: (Pair a -> Pair b) :&: Hom (Pair (Pair a)) (Pair (Pair b))
  ...

-}
data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

-- I have to say this is a smart way of keeping track
apply :: Hom a b -> BalTree a -> BalTree b
apply (f :&: fs) t = case t of
    Zero x -> Zero (f x)
    Succ t' -> Succ (apply fs t')

arrHom :: (a -> b) -> Hom a b
arrHom f = f :&: arrHom (f *** f)

composeHom :: Hom a b -> Hom b c -> Hom a c
composeHom (a :&: as) (b :&: bs) = (b . a) :&: composeHom as bs

instance Cat.Category Hom where
    id = arrHom id
    g . f = composeHom f g

firstHom :: Hom a b -> Hom (a,d) (b,d)
firstHom (f :&: fs) = first f :&: (tr >>> firstHom fs >>> tr)
  where
    transpose :: ((a,b),(c,d)) -> ((a,c),(b,d))
    transpose ((a,b),(c,d)) = ((a,c),(b,d))
    tr = arrHom transpose

instance Arrow Hom where
    arr = arrHom
    first = firstHom

-- TODO: shifting elements?
rsh :: forall a. a -> Hom a a
rsh v = const v :&: proc (o :: a,e :: a) -> do
    o' <- rsh v -< e
    returnA -< (o',o)

-- TODO: comment
scan :: Num a => Hom a a
scan = id :&: proc (o,e) -> do
    -- pairwise scan?
    e' <- scan -< o+e
    -- shift to right
    el <- rsh 0 -< e
    returnA -< (el+o, e')
