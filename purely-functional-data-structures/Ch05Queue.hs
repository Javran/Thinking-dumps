module Ch05Queue where

type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

isEmpty :: Queue a -> Bool
isEmpty ([], _) = True -- according to the invariant
isEmpty _ = False

qHead :: Queue a -> a
qHead (x:f, r) = x

qTail :: Queue a -> Queue a
qTail (x:f, r) = checkF (f,r)

qSnoc :: Queue a -> a -> Queue a
qSnoc (f,r) x = checkF (f, x:r)

-- maintain the invariant that "f" part is empty
-- only if "r" is also empty
checkF :: Queue a -> Queue a
checkF ([], r) = (reverse r, [])
checkF v = v
