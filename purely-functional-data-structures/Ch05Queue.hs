module Ch05Queue
  ( Queue
  , empty
  , isEmpty
  , view
  , head
  , tail
  , snoc
  , toList
  ) where

import Prelude hiding (head,tail)
import Data.Maybe

-- a queue is a pair of lists (f, r), where:
-- + "f" contains the front elements
-- + "r" contains the rear elements in reversed order
-- + so "f ++ reverse r" converts the queue to a normal list
-- + it is designed in this way to have quick access to the first
--   and the last element of the collection.
-- + INVARIANT: "f" is empty only if "r" is empty.
--   in other words, "f" is always non-empty unless the whole queue is empty.
type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

isEmpty :: Queue a -> Bool
isEmpty ([], _) = True -- according to the invariant
isEmpty _ = False

-- maintain the invariant
checkF :: Queue a -> Queue a
checkF ([], r) = (reverse r, [])
checkF v = v

view :: Queue a -> Maybe (a, Queue a)
view q
    | (x:f, r) <- q = Just (x, checkF (f,r))
    | otherwise = Nothing

head :: Queue a -> a
head = fst . fromJust . view

tail :: Queue a -> Queue a
tail = snd . fromJust . view

snoc :: Queue a -> a -> Queue a
snoc (f,r) x = checkF (f, x:r)

toList :: Queue a -> [a]
toList (f,r) = f ++ reverse r

{-

"tail" and "snoc" both take O(1) amortized time.

Banker's method:

* when "snoc" on an non-empty list, in addition to the actual O(1) operation,
  we assign an extra credit to that element.
* whenever a "reverse" is triggered, that extra credit is spent to cover the cost.

Physicst's method:

* define the potential to be the length of the rear list

-}
