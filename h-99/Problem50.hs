module Problem50 where

import qualified Data.Heap as H
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Function

data BinTree a
    = Leaf Int a
    | Node Int (BinTree a) (BinTree a)
      deriving (Show)

getWeight :: BinTree a -> Int
getWeight (Leaf w _) = w
getWeight (Node w _ _) = w

mergeBin :: BinTree a -> BinTree a -> BinTree a
mergeBin l r = Node (((+) `on` getWeight) l r) l r

instance Eq (BinTree a) where
    (==) = (==) `on` getWeight

instance Ord (BinTree a) where
    compare = compare `on` getWeight

huffman :: Ord a => [(a, Int)] -> [(a,String)]
huffman = sortBy (compare `on` fst)
        . buildEncodeTable
        . mkHuffmanTree
        . H.fromList
        . map (uncurry (flip Leaf))

buildEncodeTable :: BinTree a -> [(a,String)]
buildEncodeTable (Leaf _ c) = [(c,[])]
buildEncodeTable (Node _ l r) =
   map (second ('0':)) (buildEncodeTable l) ++
   map (second ('1':)) (buildEncodeTable r)

mkHuffmanTree :: Ord a => H.MinHeap (BinTree a) -> BinTree a
mkHuffmanTree hp
    | H.null hp = error "making a tree from nothing"
    | H.size hp == 1 = fromJust $ H.viewHead hp
    | otherwise =
        let (h1:h2:_,newHp) = H.splitAt 2 hp
        in mkHuffmanTree (H.insert (mergeBin h1 h2) newHp)

main :: IO ()
main = print $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
