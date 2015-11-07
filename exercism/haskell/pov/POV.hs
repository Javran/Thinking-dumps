module POV
  ( Graph(..)
  , fromPOV
  , tracePathBetween
  ) where

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
    -- (<visited>, <rest of the elements>)
  , bothers :: ([Graph a], [Graph a])
  }

fromPOV :: Eq a => a -> Graph a -> Maybe (Graph a)
fromPOV = undefined

tracePathBetween :: Eq a => a -> a -> Graph a -> Maybe [a]
tracePathBetween = undefined
