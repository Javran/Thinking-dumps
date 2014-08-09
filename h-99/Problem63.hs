module Problem63
where

import Control.Arrow
import qualified Data.Sequence as Seq
import Data.Sequence (viewl,ViewL(..),(|>))
import BinaryTree

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = reduce (Seq.fromList bottomLayer)
    where
        h :: Int
        h = floor (logBase (2::Double) (fromIntegral (n + 1)))
        nodes = n - (2^h - 1)
        bottomLayer = take (2^h) (replicate nodes (leaf 'x') ++ repeat Empty)
        reduce :: Seq.Seq (Tree Char) -> Tree Char
        reduce s = case viewl s of
            EmptyL -> error "impossible"
            (t1 :< rest1) -> case viewl rest1 of
                EmptyL -> t1
                (t2 :< rest2) -> reduce (rest2 |> Branch 'x' t1 t2)

main :: IO ()
main = mapM_ (print . (id &&& completeBinaryTree)) [0..10]
