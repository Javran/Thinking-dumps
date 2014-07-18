module Problem07 where

data NestedList a
    = Elem a
    | List [NestedList a]
      deriving (Show, Eq)

flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List xs) = concatMap flatten xs

main :: IO ()
main = do
    let t1, t2, t3 :: NestedList Int
        t1 = Elem 5
        t2 = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
        t3 = List []
    mapM_ (print . flatten) [t1, t2, t3]
