module Problem65 where

import BinaryTree
import Control.Arrow
import qualified Data.Foldable as FD

tree65 :: Tree Char
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)))
                        (Branch 'm' Empty Empty))
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty))
                        Empty)

layout :: Tree Char -> Tree (Char,(Int,Int))
layout t = normalize (layout' t (2 ^ depth t))
    where
      -- | make sure the leftmost col coord is always "1"
      normalize Empty = Empty
      -- | leftMostCol-(leftMostCol-1) = 1
      normalize t1 = fmap (second (first (subtract (leftMostCol - 1)))) t1
          where
            leftMostCol = FD.minimum (fmap (snd >>> fst) t1)
      layout' Empty _ = Empty
      layout' (Branch v l r) dist =
          Branch (v,(centerCol,1))
                 (fmap (second (second (+1)))       layoutL)
                 (fmap (second ((+ dist) *** (+1))) layoutR)
          where
            half = dist `div` 2
            centerCol = dist
            layoutL = layout' l half
            layoutR = layout' r half

main :: IO ()
main = print . layout $ tree65
