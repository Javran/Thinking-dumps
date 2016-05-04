module Ch03Exercise4 where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
{-# ANN module "HLint: ignore Redundant do" #-}


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
