module Ch03RedBlack where

import Control.Monad
import Data.Maybe

data Color = Red | Black deriving (Eq, Show)

data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show)

empty :: Tree a
empty = E

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
balance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d)
balance c l v r = T c l v r

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T Black a y b
  where
    ins E = T Red E x E
    ins s1@(T c1 a1 y1 b1)
        | x < y1 = balance c1 (ins a1) y1 b1
        | x > y1 = balance c1 a1 y1 (ins b1)
        | otherwise = s1
    (T _ a y b) = ins s

toAscList :: Tree a -> [a]
toAscList E = []
toAscList (T _ l v r) = toAscList l ++ v : toAscList r

color :: Tree a -> Color
color E = Black
color (T c _ _ _) = c

-- count the number of black nodes from root to nil.
-- any violation of the black node depth property should result in failure
countBlackDepth :: Tree a -> Maybe Int
countBlackDepth E = Just 1 -- empty nodes are considered black
countBlackDepth (T c l _ r) = do
    lDep <- countBlackDepth l
    rDep <- countBlackDepth r
    guard $ lDep == rDep
    case c of
        Black -> pure (lDep + 1)
        Red -> pure lDep

checkColorProperty' :: Tree a -> Maybe ()
checkColorProperty' E = Just ()
checkColorProperty' (T c l _ r) = do
    checkColorProperty' l
    checkColorProperty' r
    case c of
        Black -> pure ()
        Red -> guard $ color l /= Red && color r /= Red

checkColorProperty :: Tree a -> Bool
checkColorProperty = isJust . checkColorProperty'
