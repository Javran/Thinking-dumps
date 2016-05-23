module Ch05Queue where

type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

isEmpty :: Queue a -> Bool
isEmpty ([], []) = True
isEmpty _ = False

qHead :: Queue a -> a
qHead (x:f, r) = x

qTail :: Queue a -> Queue a
qTail (x:f, r) = (f,r)

qSnoc :: Queue a -> a -> Queue a
qSnoc (f,r) x = (f, x:r)
