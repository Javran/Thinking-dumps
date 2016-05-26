module Ch05Exercise1 where

type Deque a = ([a], [a])

empty :: Deque a
empty = ([], [])

isEmpty :: Deque a -> Bool
isEmpty ([], []) = True
isEmpty _ = False

checkDq :: Deque a -> Deque a
checkDq dq = case dq of
    ([], []) -> dq
    -- when front is empty
    ([], r) -> case r of
        [_] -> (r,[])
        -- cut rear into half
        _ | (r1,r2) <- half r
            -> (reverse r2, r1)
    -- when rear is empty
    (f, []) -> case f of
        [_] -> dq
        -- cut front into half
        _ | (f1,f2) <- half f
            -> (f1, reverse f2)
    _ -> dq

half :: [a] -> ([a],[a])
half xs = splitAt (length xs `div` 2) xs
