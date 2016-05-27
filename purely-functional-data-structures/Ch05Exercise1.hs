module Ch05Exercise1 where

import Data.Maybe

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

viewHead :: Deque a -> Maybe (a, Deque a)
viewHead ([],[]) = Nothing
viewHead (x:f,r)= Just (x, checkDq (f,r))
viewHead _ = error "invariant violated"

viewLast :: Deque a -> Maybe (a, Deque a)
viewLast ([],[]) = Nothing
viewLast ([v],[]) = Just (v,empty)
viewLast (f,x:r) = Just (x, checkDq (f,r))
viewLast _ = error "invariant violated"

head :: Deque a -> a
head = fst . fromJust . viewHead

tail :: Deque a -> Deque a
tail = snd . fromJust . viewHead

last :: Deque a -> a
last = fst . fromJust . viewLast

init :: Deque a -> Deque a
init = snd . fromJust . viewLast

cons :: a -> Deque a -> Deque a
cons v (f,r) = checkDq (v:f,r)

snoc :: Deque a -> a -> Deque a
snoc (f,r) v = checkDq (f,v:r)
