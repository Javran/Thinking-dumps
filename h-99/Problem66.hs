module Problem66 where

import Control.Arrow
import Data.Function

import qualified Data.Set as S
import qualified Data.Foldable as FD

import BinaryTree
import Problem65 (tree65, normalizeTreeCoord)

layout :: Tree Char -> Tree (Char,(Int,Int))
layout = normalizeTreeCoord . layout'
    where
      layout' Empty = Empty
      layout' (Branch v l r) =
          Branch (v,(0,1))
                 (fmap (second ((subtract half  *** (+1)))) layoutL)
                 (fmap (second (((+       half) *** (+1)))) layoutR)
          where
            -- minimize the distance between two subtrees so that
            -- they don't overlap
            properDist = head
                       . dropWhile (hasOverlap layoutL layoutR)
                       $ [2,4..]
            half = properDist `div` 2
            layoutL = layout' l
            layoutR = layout' r
            toCoords :: Tree (a,(Int,Int)) -> S.Set (Int,Int)
            toCoords = S.fromList . FD.toList . fmap snd
            hasOverlap :: Tree (a,(Int,Int)) -> Tree (a,(Int,Int)) -> Int -> Bool
            hasOverlap t1 t2 offset = not
                                    . S.null
                                    $ (S.intersection `on` toCoords)
                                        t1
                                        -- apply offset on the right subtree
                                        (fmap (second (first (+ offset))) t2)

main :: IO ()
main = print . layout $ tree65
