module Ch02Exercise3 where

data BST a = E | T (BST a) a (BST a) deriving (Show)

-- the proper way of implementing "exception" is to make use of Maybe
-- let's try doing it.
insertM :: Ord a => a -> BST a -> Maybe (BST a)
insertM v E = Just $ T E v E
insertM v (T l x r)
    | v == x = Nothing
    | v <  x = (\newL -> T newL x r) <$> insertM v l
    | v >  x = T l x <$> insertM v r
    | otherwise = error "impossible"
