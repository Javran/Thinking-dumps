module Problem67A where

import BinaryTree
import Control.Applicative
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import qualified Data.Foldable as FD
import Text.Printf

parseTree :: Parser (Tree Char)
parseTree =  nonEmpty
         <|> return Empty
    where
      nonEmpty = do
          v <- letter
          children <- (do char '('
                          l <- parseTree
                          char ','
                          r <- parseTree
                          char ')'
                          return (Just (l,r)))
                  <|> return Nothing
          return (maybe
                  (singleton v)
                  (uncurry (Branch v))
                  children)

stringToTree :: String -> Tree Char
stringToTree s = case parse parseTree "" s of
    Right t -> t
    Left m -> error (show m)

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch v Empty Empty) = [v]
treeToString (Branch v l r) = printf "%s(%s,%s)"
                                     [v]
                                     (treeToString l)
                                     (treeToString r)

tree67 :: Tree Char
tree67 = stringToTree "a(b(d,e),c(,f(g,)))"

main :: IO ()
main = do
    print tree67
    print (treeToString (stringToTree "x(y,a(,b))"))
    let t1 = cbtFromList ['a'..'z']
        t2 = stringToTree . treeToString $ t1
    print t1
    print t2
    print $ t1 == t2

-- | copied from https://www.haskell.org/haskellwiki/99_questions/Solutions/63
cbtFromList :: [a] -> Tree a
cbtFromList ys = let (t, xss) = cbt (ys:xss) in t
  where cbt ((x:xs):xss) =
                let (l, xss') = cbt xss
                    (r, xss'') = cbt xss'
                in  (Branch x l r, xs:xss'')
        cbt _ = (Empty, [])
