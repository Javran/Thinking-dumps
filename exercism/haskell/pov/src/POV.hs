module POV
  ( TreeContext (..)
  , fromPOV
  , tracePathBetween
  )
where

import Data.List
import Data.Tree

{-
  To solve this problem, we can use zipper, which contains
  enough information for us to rebuild the tree from current focus
-}

data TreeContext a = GContext
  { parentTag :: a
  , -- (<visited or visiting>, <rest of the elements>)
    -- INVARIANT: fst of "bothers" must not be empty
    bothers :: ([Tree a], [Tree a])
  }
  deriving (Show)

-- | create list zippers: [1,2,3] -> [([1],[2,3]), ([2,1],[3]), ([3,2,1],[])]
--   note that the first element of "fst" also contains the focus,
--   so it cannot be empty
listZippers :: [a] -> [([a], [a])]
listZippers cs = unfoldr f ([], cs)
  where
    f (st, xs) = case xs of
      [] -> Nothing
      (y : ys) -> let v = (y : st, ys) in Just (v, v)

-- | get all zipppers of immediate children of a tree
getChildZippers :: Tree a -> [(Tree a, TreeContext a)]
getChildZippers (Node t cs) = map mkZipper subZippers
  where
    subZippers = listZippers cs
    mkZipper (x : xs, ys) = (x, GContext t (xs, ys))
    mkZipper _ = error "invalid listZipper"

-- | given a zipper (with possibly stacked contexts)
--   return all zippers for all of its subnodes (the tree itself is included)
getAllZippers :: (Tree a, [TreeContext a]) -> [(Tree a, [TreeContext a])]
getAllZippers curZipper@(g, gcs) =
  curZipper :
  concatMap expand (getChildZippers g)
  where
    expand (g1, gc1) = getAllZippers (g1, gc1 : gcs)

rebuildFromZipper :: (Tree a, [TreeContext a]) -> Tree a
rebuildFromZipper (t, []) = t
rebuildFromZipper (Node tg children, GContext pt (bsL, bsR) : cs) =
  Node tg (children ++ [ps])
  where
    ps = rebuildFromZipper (Node pt (reverse bsL ++ bsR), cs)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV v t =
  rebuildFromZipper
    <$>
    -- traverse all possible zippers and find the one with
    -- correct tag value, and then rebuild a tree from it
    find
      ((== v) . rootLabel . fst)
      (getAllZippers (t, []))

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween fromTag toTag t = do
  -- reconstruct the tree so that node with "fromTag"
  -- becomes the root
  povTree <- fromPOV fromTag t
  -- based on reconstructed tree, we can find the zipper
  -- along whose context we are able to recover the path
  (Node destTag _, cs) <-
    find
      ((== toTag) . rootLabel . fst)
      (getAllZippers (povTree, []))
  -- extract tags from the stack of contexts, and attach destionation tag to it
  -- so we'll have the full path
  return $ foldl' (\acc ctxt -> parentTag ctxt : acc) [destTag] cs
