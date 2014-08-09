module Problem64
where

import BinaryTree
import Control.Monad.State

layout :: Tree a -> Tree (a,(Int,Int))
layout t = evalState (layoutAux t 1) 1
    where
        layoutAux Empty _ = return Empty
        layoutAux (Branch v l r) n = do
            lNode <- layoutAux l (n+1)
            ind <- get
            modify (+1)
            rNode <- layoutAux r (n+1)
            return $ Branch (v,(ind,n)) lNode rNode

tree64 :: Tree Char
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty)
                                        Empty))
                        (Branch 'm' Empty Empty))
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty))
                        Empty)

main :: IO ()
main = print $ layout tree64
