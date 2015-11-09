module POV
  ( Graph(..)
  , fromPOV
  , tracePathBetween
  ) where

import Data.List

-- I'm still not sure why this is called "Graph" when
-- it's actually a tree-representation
data Graph a = Graph
  { gTag :: a
  , gChildren :: [Graph a]
  } deriving (Eq, Show)

{-
  To solve this problem, we can use zipper, which contains
  enough information for us to rebuild the tree from current focus
-}

data GraphContext a = GContext
  { parentTag :: a
    -- (<visited or visiting>, <rest of the elements>)
    -- INVARIANT: fst of "bothers" must not be empty
  , bothers :: ([Graph a], [Graph a])
  } deriving (Show)

-- | create list zippers: [1,2,3] -> [([1],[2,3]), ([2,1],[3]), ([3,2,1],[])]
--   note that the first element of "fst" also contains the focus,
--   so it cannot be empty
listZippers :: [a] -> [([a],[a])]
listZippers cs = unfoldr f ([],cs)
  where
    f (st,xs) = case xs of
        [] -> Nothing
        (y:ys) -> let v = (y:st,ys) in Just (v,v)

-- | get all zipppers of immediate children of a tree
getChildZippers :: Graph a -> [(Graph a, GraphContext a)]
getChildZippers (Graph t cs) = map mkZipper subZippers
  where
    subZippers = listZippers cs
    mkZipper (x:xs, ys) = (x, GContext t (xs,ys))
    mkZipper _ = error "invalid listZipper"

-- | given a zipper (with possibly stacked contexts)
--   return all zippers for all of its subnodes (the tree itself is included)
getAllZippers :: (Graph a, [GraphContext a]) -> [(Graph a, [GraphContext a])]
getAllZippers curZipper@(g,gcs) =
    curZipper
    : concatMap expand (getChildZippers g)
  where
    expand (g1, gc1) = getAllZippers (g1, gc1:gcs)

rebuildFromZipper :: (Graph a, [GraphContext a]) -> Graph a
rebuildFromZipper (t,[]) = t
rebuildFromZipper (Graph tg children,GContext pt (bsL,bsR):cs) =
    Graph tg (children ++ [ps])
  where
    ps = rebuildFromZipper (Graph pt (reverse bsL ++ bsR), cs)

fromPOV :: Eq a => a -> Graph a -> Maybe (Graph a)
fromPOV v t = rebuildFromZipper <$>
                -- traverse all possible zippers and find the one with
                -- correct tag value, and then rebuild a tree from it
                find
                  ((== v) . gTag . fst)
                  (getAllZippers (t,[]))

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween fromTag toTag t = do
    -- reconstruct the tree so that node with "fromTag"
    -- becomes the root
    povTree <- fromPOV fromTag t
    -- based on reconstructed tree, we can find the zipper
    -- along whose context we are able to recover the path
    (Graph destTag _, cs) <- find
                               ((== toTag) . gTag . fst)
                               (getAllZippers (povTree,[]))
    return (recoverPath [destTag] cs)
  where
    recoverPath acc [] = acc
    recoverPath acc (GContext tg _:cs) = recoverPath (tg : acc) cs
