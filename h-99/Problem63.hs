module Problem63
where

import Control.Applicative
import qualified Data.Sequence as Seq
import Data.Sequence (viewl,ViewL(..),(|>))
import Text.Printf
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

linearize :: Tree a -> [Tree a]
linearize = concat . toLayers
    where
        -- break tree into layers
        toLayers Empty = [[Empty]]
        toLayers t@(Branch _ l r) = [t] : zipWith (++) (toLayers l) (toLayers r)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree = all isEmpty . dropWhile (not . isEmpty) . linearize

main :: IO ()
main = do
    mapM_ (printf "%d => %s\n" <*> show . completeBinaryTree) [0..10]
    print (all (isCompleteBinaryTree . completeBinaryTree) [0..100])
    print $ completeBinaryTree 4
    print $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
    print $ isCompleteBinaryTree Empty
    print $ isCompleteBinaryTree tree4
