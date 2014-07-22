module Problem50 where

import qualified Data.Heap as H
import Data.Maybe

data BinTree a
    = Leaf Int a
    | Node Int (BinTree a) (BinTree a)
      deriving (Show)

huffman :: Ord a => [(a, Int)] -> [(a,String)]
huffman xs = undefined

mkHuffmanTree :: Ord a => H.MinHeap (BinTree a) -> BinTree a
mkHuffmanTree hp
    | H.null hp = error "making a tree from nothing"
    | H.size == 1 = fromJust $ H.viewHead hp

