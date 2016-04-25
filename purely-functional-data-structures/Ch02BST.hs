module Ch02BST
  ( BST(..)
  , makeFromList
  , toAscList
  , member
  , insert
  ) where

-- common functions of BST to be used for exercises
data BST a = E | T (BST a) a (BST a) deriving (Show)

-- the "member" function we all know
member :: Ord a => a -> BST a -> Bool
member _ E = False
member v (T l x r)
    | v < x = member v l
    | v > x = member v r
    | otherwise = v == x

insert :: Ord a => a -> BST a -> BST a
insert x E = T E x E
insert x s@(T l y r)
    | x < y = T (insert x l) y r
    | x > y = T l y (insert x r)
    | otherwise = s

-- make "fromList" function by giving an "insert" function of the correct type
makeFromList :: Ord a => (a -> BST a -> BST a) -> [a] -> BST a
makeFromList insert' = foldr insert' E

toAscList :: BST a -> [a]
toAscList E = []
toAscList (T l v r) = toAscList l ++ v : toAscList r
