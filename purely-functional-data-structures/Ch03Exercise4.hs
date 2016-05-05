module Ch03Exercise4 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
{-# ANN module "HLint: ignore Redundant do" #-}

{-

(a) prove that the right spine of a weighted-biased leftist heap
    contains at most floor(log(n+1)) elements.

I think the proof goes the same way as what we have done for exercise 3.1.

First we can prove a lemma: if the right spine has r nodes,
then the tree has at least 2^r-1 nodes.

base case: r=1. true

induction step:

* assume when r=k, the tree has at least 2^k-1 nodes,

* and we want to prove that when r = k+1, the tree will have at least 2^(k+1)-1 nodes:

ignoring the root, and looking at both subtrees, they are both valid weight-biased
leftist trees, and the right subtree has exactly k nodes on its right spine.
thus right subtree has at least 2^k-1 nodes according to the assumption.
further, according to the property of weight-biased leftist heap,
left subtree should also has 2^k-1 nodes.

putting these two facts together we know this tree has (2^k-1)+1+(2^k-1) = 2^(k+1)-1 nodes.

therefore the conclusion.

after this lemma is proved, everything goes exactly the same as the proof
we have in exercise 3.1. the proof is informal so I'm not going to repeat it again.

-}


-- the difference between leftist heap and
-- weight-biased leftist heap is not too large

data Heap a = E | T Int a (Heap a) (Heap a)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

rank :: Heap a -> Int
rank E = 0
rank (T rnk _ _ _) = rnk

singleton :: a -> Heap a
singleton x = T 1 x E E

makeT :: Ord a => a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
    then T newRank x a b
    else T newRank x b a
  where
    -- unlike leftist heap,
    -- in weighted biased version
    -- the rank is the size of the tree
    newRank = rank a + rank b + 1

-- seems we can keep "merge" the same
-- as it has nothing to do with the rank we are modifying
merge :: Ord a => Heap a -> Heap a -> Heap a
merge l E = l
merge E r = r
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x <= y
    then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge h1 b2)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (singleton x)

viewMin :: Ord a => Heap a -> Maybe (a, Heap a)
viewMin E = Nothing
viewMin (T _ x a b) = Just (x, merge a b)

findMin :: Ord a => Heap a -> Maybe a
findMin = fmap fst . viewMin

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin = fmap snd . viewMin

toAscList :: Ord a => Heap a -> [a]
toAscList h = case viewMin h of
    Nothing -> []
    Just (v,newH) -> v : toAscList newH

sortByHeap :: Ord a => [a] -> [a]
sortByHeap = toAscList . foldr insert empty

main :: IO ()
main = hspec $ do
    describe "Weight-biased Leftist" $ do
      it "can sort elements" $ do
        property $ \xs -> sortByHeap (xs :: [Int]) == L.sort xs
