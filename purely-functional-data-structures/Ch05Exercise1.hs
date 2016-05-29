module Ch05Exercise1
  ( Deque
  , empty
  , isEmpty

  , cons
  , snoc

  , head
  , tail
  , last
  , init

  , toList
  ) where

import Prelude hiding (head,tail,last,init)
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

toList :: Deque a -> [a]
toList (f,r) = f ++ reverse r

{-


let's assume the deque is not empty:

when len f >= len r, abs(len f - len r) = len f - len r.
* inserting in front of the deque increases the potential by 1.
* because "f" is no shorter than "r" and the deque is not empty, it's safe to say that
  a split of the rear list will never happen under this case.
* if removing one element from the beginning of the deque does not cause a split
  of the rear list, the potential decreases by 1.

when len f < len r, abs(len f - len r) = len r - len f
* inserting in front of the deque decreases the potential by 1
* if removing one element from the beginning of the deque does not cause a split
  of the rear list, the potential increases by 1.
* if removing one element does cause a split, the opeartion takes "len r + len r / 2" time
  (because we need to traverse the list to count elements and then reverse half of it.
   but we can keep track of the list length to make it just "len r/2").
  potential before the operation: len f - len r, where len f = 1 (the only case where
  removing an element would cause a split)
  potential after the operation: len r/2 - len r/2 = 0 or 1 (r might be odd, so the
  potential is only an estimation)

TODO

I feel this is a rather weird potential function using abs(len f - len r),
as inserting into a non-empty deque could either increase or decrease the potential.
I'm not sure how to deal with this situation for now.

-}
