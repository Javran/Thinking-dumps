module Problem70 where

import MTree
import Text.Parsec hiding ((<|>),many)
import Text.Parsec.String (Parser)
import Control.Applicative

treeToString :: Tree Char -> String
treeToString (Node v sub) = v: concatMap treeToString sub ++ "^"

stringToTree :: String -> Tree Char
stringToTree s = case parse parseTree "" s of
    Left msg -> error (show msg)
    Right t -> t
  where
    parseTree = Node <$> satisfy (/='^')
                     <*> (many parseTree <* char '^')

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawTree . fmap show

main :: IO ()
main = do
    let example = "afg^^c^bd^e^^^"
    printTree $ stringToTree example
    print $ treeToString (stringToTree example)
    print $ example == (treeToString . stringToTree) example
    print $ stringToTree example == (stringToTree . treeToString . stringToTree) example
