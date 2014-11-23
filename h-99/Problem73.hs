module Problem73 where

import MTree
import Text.Parsec hiding ((<|>))
import Control.Applicative

{-
  I don't understand why the example is given in this way:

  Tree> display lisp tree1
  "a"
  Tree> display lisp tree2
  "(a b)"

  why not just keep the same way as before:
  a function that converts string to tree,
  and another that converts the other way around
-}

tree2Lispy :: Tree Char -> String
tree2Lispy (Node v []) = [v]
tree2Lispy (Node v subs) = "("
                        ++ unwords ([[v]] ++ map tree2Lispy subs)
                        ++ ")"

lispy2Tree :: String -> Tree Char
lispy2Tree s = case parse parseTree "" s of
    Left msg -> error (show msg)
    Right t -> t
  where
    parseTree = (between (char '(')
                         (char ')')
                         (parseTree `sepBy1` space) >>= \(Node x []:xs) ->
                 return (Node x xs))
             <|> Node <$> anyChar <*> pure []

main :: IO ()
main = do
    print (tree2Lispy tree5)
    print $ tree5 == (lispy2Tree . tree2Lispy) tree5
    print $ tree2Lispy tree5 == ( tree2Lispy
                                . lispy2Tree
                                . tree2Lispy
                                ) tree5
