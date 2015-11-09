module POV
  ( Graph(..)
  , fromPOV
  , tracePathBetween
  ) where

import Data.List

-- I'm still not sure why this is called "Graph" when
-- it's actually a tree-representation
data Graph a
  = Graph a [Graph a]
  deriving (Eq, Show)

{-
  plan: we need a zipper, as we walk through
  the tree, we will try to find the tag, at that time
  some context will be accumulated, which includes enough
  information for us to construct the new tree..
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
fromPOV = undefined

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween = undefined
