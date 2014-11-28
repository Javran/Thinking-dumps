module Problem72 where

import MTree

bottomUp :: Tree a -> [a]
bottomUp (Node v subs) = concatMap bottomUp subs ++ [v]

main :: IO ()
main = print (bottomUp tree5)
