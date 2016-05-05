module Ch03Leftist where

import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
{-# ANN module "HLint: ignore Redundant do" #-}

data Heap a = E | T Int a (Heap a) (Heap a)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge l E = l
merge E r = r
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x <= y
    then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge h1 b2)

rank :: Heap a -> Int
rank E = 0
rank (T rnk _ _ _) = rnk

-- it's not necessary to have constraints on "a"
-- because the value is not checked at all.
-- we only interested in putting one with less rank
-- to the right.
makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
  then T (rank b+1) x a b
  else T (rank a+1) x b a

singleton :: a -> Heap a
singleton x = T 1 x E E

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
    describe "Leftist" $ do
      it "can sort elements" $ do
        property $ \xs -> sortByHeap (xs :: [Int]) == L.sort xs
