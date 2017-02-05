{-# LANGUAGE GADTs #-}
module FTCQueue1 where

data FTCQueue m a b where
    Leaf :: (a -> m b) -> FTCQueue m a b
    Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b -- composition?

tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)

(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(><) = Node

data ViewL m a b where
    TOne :: (a -> m b) -> ViewL m a b
    (:|) :: (a -> m x) -> FTCQueue m x b -> ViewL m a b

tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
  where
    go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
    go (Leaf r) tr = r :| tr
    -- this gives the left-leaning behavior:
    -- as we go deeper into the tree, more and more elements are pushed to right
    go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

plusPrint :: Int -> IO Int
plusPrint n = print n >> pure (succ n)

test1 :: FTCQueue IO Int Int
test1 = tsingleton plusPrint

-- support appending on right (snoc)
test2 :: FTCQueue IO Int Int
test2 = test1 |> plusPrint

-- and merging two
test3 :: FTCQueue IO Int Int
test3 = test2 >< test2
