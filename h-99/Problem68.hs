module Problem68 where

import BinaryTree
import Problem67A (stringToTree,tree67)
import qualified Data.Foldable as FD
import Data.Function
import Data.List

treeToPreorder :: Tree a -> [a]
treeToPreorder = FD.toList

treeToInorder :: Tree a -> [a]
treeToInorder Empty = []
treeToInorder (Branch v l r) = treeToInorder l ++ [v] ++ treeToInorder r

preInTree :: Eq a => [a] -> [a] -> Tree a
preInTree seqP seqI
    | null seqP && null seqI = Empty
    | (a:as) <- seqP
    , (b:bs) <- seqI
    , (newIL,_:newIR) <- break (== a) seqI
    , lenL <- length newIL
    , (newPL,newPR) <- splitAt lenL . tail $ seqP
    = Branch a (preInTree newPL newIL) (preInTree newPR newIR)

main :: IO ()
main = do
    let t = tree67
        po = treeToPreorder t
        io = treeToInorder t
    -- (a) preorder and inorder
    putStrLn po
    putStrLn io
    -- (b) preorder cannot be used in the reverse direction
    -- because a particular preorder sequence
    -- might correspond to multiple binary trees
    let t1 = Branch 'a' (singleton 'b') (singleton 'c')
        t2 = Branch 'a' (Branch 'b' Empty (singleton 'c')) Empty
    -- different trees
    print $ t1 == t2
    -- same preorder
    print $ ((==) `on` treeToPreorder) t1 t2
    -- (c)
    let t' = preInTree po io
    print t'
    print $ t == t'
