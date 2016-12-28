{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Hom where

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad.State
import Data.Tuple

-- homogeneous functions

{-
some links that might be helpful:
(for the rest of this file, reference can take the form of "[x]",
and that means a link in the following list.)

[1] https://en.wikipedia.org/wiki/Bitonic_sorter
[2] https://en.wikipedia.org/wiki/Butterfly_network
[3] http://code.haskell.org/~ross/arrowp/examples/powertrees/Hom.las

-}

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

-- an example tree for ghci interactions
treeA :: BalTree Char
treeA = makeTree 3 ['a'..]

exTree :: Enum e => Int -> BalTree e
exTree h = makeTree h (map toEnum [0..])

exTree1,exTree2,exTree3,exTree4 :: BalTree Int

exTree1 = exTree 1
exTree2 = exTree 2
exTree3 = exTree 3
exTree4 = exTree 4

{-
> treeA
Succ (Succ (Succ (Zero ((('a','b'),('c','d')),(('e','f'),('g','h'))))))

"arr f" applies "f" to every elements of the tree:

> apply (arr succ) treeA
Succ (Succ (Succ (Zero ((('b','c'),('d','e')),(('f','g'),('h','i'))))))

-}

type Pair a = (a,a)

{-
  seems we can expand it indefinitely:

  Hom a b
  <=> (a -> b) :&: Hom (Pair a) (Pair b)
  <=> (a -> b) :&: (Pair a -> Pair b) :&: Hom (Pair (Pair a)) (Pair (Pair b))
  ...

  also notices that effectively "nextHom" and "apply" are accessors of Hom:

  nextHom :: Hom a b -> Hom (Pair a) (Pair b)
  apply :: Hom a b -> (BalTree a -> BalTree b)

-}
data Hom a b = (a -> b) :&: Hom (Pair a) (Pair b)

nextHom :: Hom a b -> Hom (Pair a) (Pair b)
nextHom (_ :&: h) = h

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
    tr = arrHom transpose

transpose :: ((a,b),(c,d)) -> ((a,c),(b,d))
transpose ((a,b),(c,d)) = ((a,c),(b,d))

instance Arrow Hom where
    arr = arrHom
    first = firstHom

-- example: apply (rsh 'x') (('a','b'),('c','d')) => (('x','a'),('b','c'))
-- see "rsh1" for a translated version
rsh :: forall a. a -> Hom a a
rsh v = rshArr
  where
    rshArr = const v :&: proc (o :: a,e :: a) -> do
        o' <- rshArr -< e
        returnA -< (o',o)

{-

a formal proof might be overkill, but this is what roughly happened:

for an input of 8 elements:

a,b c,d e,f g,h

=> arr swap

b,a d,c f,e h,g

(part first: b,d,f,h, second: a,c,e,g)

assume "rsh v" does the right thing: b,d,f,h >- rsh v -> v,b,d,f

=> first (rsh v) turns "b,a d,c f,e h,g" into "v,a b,c d,e f,g"

so consider every pair of (o,e) in the input: output of "even" wire comes
directly from an "odd" wire, the correctness is easily established.

for "odd" wires, we just hope "rsh v" on them does the right thing - so we
just need to prove by induction. the base case is "x >- rsh v -> v" and so that's correct.

-}
rsh1 :: a -> Hom a a
rsh1 v = rshArr
  where
    rshArr =
        const v :&: (arr swap >>> first rshArr)

-- might be super related: http://code.haskell.org/~ross/arrowp/examples/powertrees/Hom.las

{-
input: x1,x2,x3,x4,...
want: x1,x1+x2,x1+x2+x3,x1+x2+x3+x4,...

steps:
- prepare x1+x2, x3+x4, ...
- recursively do: (x1+x2),(x1+x2)+(x3+x4),... (1)
- prepare: x1, x3, x5 ... and 0,x1+x2,x1+x2+x3+x4
- pairwise adding to get: x1, x1+x2+x3, x1+x2+x3+x4+x5,... (2)
- put two parts (1) and (2) together
-}
scan :: Num a => Hom a a
scan = id :&: proc (o,e) -> do
    -- pairwise scan?
    e' <- scan -< o+e
    -- shift to right
    el <- rsh 0 -< e'
    returnA -< (el+o, e')

{-
> makeTree 3 [1..]
Succ (Succ (Succ (Zero (((1,2),(3,4)),((5,6),(7,8))))))
> apply scan (makeTree 3 [1..])
Succ (Succ (Succ (Zero (((1,3),(6,10)),((15,21),(28,36))))))
-}

butterfly :: (Pair a -> Pair a) -> Hom a a
butterfly f = id :&: proc (o,e) -> do
    o' <- butterfly f -< o
    e' <- butterfly f -< e
    returnA -< f (o',e')

buffertly :: (Pair a -> Pair a) -> Hom a a
buffertly f = bt
  where
    bt = id :&: (bt *** bt >>> arr f)

{-
> makeTree 3 ['a'..]
Succ (Succ (Succ (Zero ((('a','b'),('c','d')),(('e','f'),('g','h'))))))
> apply (butterfly (\(a,b) -> (b,a))) (makeTree 3 ['a'..])
Succ (Succ (Succ (Zero ((('h','g'),('f','e')),(('d','c'),('b','a'))))))
> apply (butterfly (\(a,b) -> (succ b,succ a))) (makeTree 3 ['a'..]) -- succ is called 3 times?
Succ (Succ (Succ (Zero ((('k','j'),('i','h')),(('g','f'),('e','d'))))))
-}

rev :: Hom a a
rev = butterfly swap

{-
effect of "unriffle":

> makeTree 3 (iterate (succ *** succ) (0,0))
Succ (Succ (Succ (Zero ((((0,0),(1,1)),((2,2),(3,3))),(((4,4),(5,5)),((6,6),(7,7)))))))
> apply unriffle $ makeTree 3 (iterate (succ *** succ) (0,0))
Succ (Succ (Succ (Zero ((((0,1),(2,3)),((4,5),(6,7))),(((0,1),(2,3)),((4,5),(6,7)))))))

-}
unriffle :: Hom (Pair a) (Pair a)
unriffle = butterfly transpose

bisort :: Ord a => Hom a a
bisort = butterfly cmp
  where
    cmp (x,y) = case x `compare` y of
        LT -> (x,y)
        EQ -> (x,y)
        GT -> (y,x)


{-
> makeTree 3 [1,8,3,7,2,6,4,5]
Succ (Succ (Succ (Zero (((1,8),(3,7)),((2,6),(4,5))))))
> apply bisort (makeTree 3 [1,8,3,7,2,6,4,5])
Succ (Succ (Succ (Zero (((1,5),(3,6)),((2,7),(4,8))))))

I may have figured out what's going on:

(1,8) (3,7) (2,6) (4,5)

- apply "f" on every pair:

(1,8) (3,7) (2,6) (4,5)

- rearrange every 2 pairs, will recover later:

(1,3) (8,7) (2,4) (6,5)

- now apply "f" on every pair:

(1,3) (7,8) (2,4) (5,6)

- recover from the arrangement:

(1,7) (3,8) (2,5) (4,6)

- now rearrage again, on a "higher level", will recover later.

(1,7) (2,5) (3,8) (4,6)

- now rearrage again, on the "lower level", will recover later.

(1,2) (7,5) (3,4) (8,6)

- apply "f":

(1,2) (5,7) (3,4) (6,8)

- recover

(1,5) (2,7) (3,6) (4,8)

- recover

(1,5) (3,6) (2,7) (4,8)

the effect of "butterfly cmp" on an input of 8 elements is like:

1 -x--x-----x-----------
   |  |     |
2 -x--|--x--|--x--------
      |  |  |  |
3 -x--x--|--|--|--x-----
   |     |  |  |  |
4 -x-----x--|--|--|--x--
            |  |  |  |
5 -x--x-----x--|--|--|--
   |  |        |  |  |
6 -x--|--x-----x--|--|--
      |  |        |  |
7 -x--x--|--------x--|--
   |     |           |
8 -x-----x-----------x--

where every vertical "x---x" thing is a "cmp" operation

-}


{-
  modified from: [3]
-}
sort :: Ord a => Hom a a
sort = id :&: proc (x,y) -> do
    x' <- sort -< x
    y' <- (rev <<< sort) -< y
    (nextHom bisort <<< unriffle) -< (x',y')

{-
  the non-arrow-notation version of "sort"
-}
sort1 :: Ord a => Hom a a
sort1 = id :&: (sort1 *** sort1 >>>
                second rev >>>
                unriffle >>>
                nextHom bisort)
