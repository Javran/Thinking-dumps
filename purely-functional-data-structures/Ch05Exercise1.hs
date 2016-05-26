module Ch05Exercise1 where

type Deque a = ([a], [a])

empty :: Deque a
empty = ([], [])

isEmpty :: Deque a -> Bool
isEmpty ([], []) = True
isEmpty _ = False

checkDq :: Deque a -> Deque a
checkDq dq@([], []) = dq
checkDq ([], [v]) = ([v],[])
checkDq dq@([_], []) = dq
checkDq (f, []) | (f1,f2) <- half f = (f1, reverse f2)
checkDq ([], r) | (r1,r2) <- half r = (reverse r2, r1)
checkDq dq = dq

half :: [a] -> ([a],[a])
half xs = splitAt (length xs `div` 2) xs
