module Problem69 where

import BinaryTree
import Problem67A (tree67)
import qualified Data.Foldable as FD
import Data.Function
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import Control.Applicative

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch v l r) = v : ((++) `on` tree2ds) l r

ds2tree :: String -> Tree Char
ds2tree s = case parse parseTree "" s of
    Right t -> t
    Left msg -> error (show msg)
    where
      parseTree =  (char '.' >> return Empty)
               <|> Branch <$> satisfy (/= '.')
                          <*> parseTree
                          <*> parseTree

main :: IO ()
main = do
    let example = tree2ds tree67
    print example
    print $ tree2ds (Branch 'x' (Branch 'y' Empty
                                            Empty)
                                (Branch 'z' (Branch '0' Empty Empty)
                                            Empty))
    print (ds2tree example)
    print $ tree67 == (ds2tree . tree2ds) tree67
