module Problem50 where

import Control.Arrow
import Data.List
import Data.Function

import qualified Leftist as L

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
        . L.fromList
        . map (uncurry (flip Leaf))

buildEncodeTable :: BinTree a -> [(a,String)]
buildEncodeTable (Leaf _ c) = [(c,[])]
buildEncodeTable (Node _ l r) =
   map (second ('0':)) (buildEncodeTable l) ++
   map (second ('1':)) (buildEncodeTable r)

mkHuffmanTree :: Ord a => L.LTree (BinTree a) -> BinTree a
mkHuffmanTree hp
    | L.null hp = error "making a tree from nothing"
    | otherwise =
        let (v1,hp1) = L.deleteMin hp
        in if L.null hp1
           then v1 -- singleton
           else let (v2,hp2) = L.deleteMin hp1
                in mkHuffmanTree (L.insert (mergeBin v1 v2) hp2)

main :: IO ()
main = print $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
