module Raz where

import Control.Monad.Random
import Data.Bits

type Level = Int
data Dir = L | R

data Tree a
  = Nil
  | Leaf a
  | Bin Level Int (Tree a) (Tree a)

data List a
  = LNil
  | LCons a (List a)
  | LLvl Level (List a)
  | LTr (Tree a) (List a)

data Zip a = Zip (List a) a (List a)

singleton :: a -> Zip a
singleton e = Zip LNil e LNil

trim :: Dir -> List a -> List a
trim d tl = case tl of
    LNil -> tl
    LCons {} -> tl
    LLvl {} -> tl
    LTr t rest ->
        let trim' h1 t1 = case h1 of
                Nil -> error "poorly formed tree"
                Leaf elm -> LCons elm t1
                Bin lv _ l r -> case d of
                    L -> trim' r (LLvl lv (LTr l t1))
                    R -> trim' l (LLvl lv (LTr r t1))
        in trim' t rest

rndLevel :: MonadRandom m => m Int
rndLevel = do
    -- provide 30 bits
    x <- getRandomR (0 :: Int, (1 `shiftL` 30)-1)
    if x == 0
       then pure 0
       else
         let loop t r =
                 {- analysis:
                    - INVARIANT: t == 2^r
                    - t is 1, 2, 4, 8, ...
                    - x consists of 30 random bits
                    - for the worse case, every bit of x is "1"
                      (so there's no way of escaping the recursive call early)
                      - x .&. t == 0 will be the case where t = 2^31 => r = 31
                      - so rndLevel outputs a number in between 0 and 31
                  -}
                 if x .&. t == 0
                   then r
                   else loop (t `shiftL` 1) (r+1)
         in pure (loop 1 0)

empty :: MonadRandom m => a -> m (Zip a)
empty n = (\x -> Zip (LLvl x (LCons n LNil)) n LNil) <$> rndLevel

insert :: MonadRandom m => Dir -> a -> Zip a -> m (Zip a)
insert d ne (Zip l e r) = case d of
    L -> (\x -> Zip (LLvl x (LCons ne l)) e r) <$> rndLevel
    R -> (\x -> Zip l e (LLvl x (LCons ne r))) <$> rndLevel

remove :: Dir -> Zip a -> Zip a
remove d (Zip l e r) = case d of
    L -> Zip (remove' L l) e r
    R -> Zip l e (remove' R r)
  where
    remove' d' s = case s of
        LNil -> error "remove past end of seq"
        LCons _ rest -> rest
        LLvl _ rest -> remove' d' rest
        LTr {} -> remove' d' (trim d' s)

viewC :: Zip a -> a
viewC (Zip _ v _) = v

-- the impl looks for the list in corresponding direction
-- and view the first element of that.
view :: Dir -> Zip a -> a
view d (Zip l _ r) = case d of
    L -> view' l
    R -> view' r
  where
    view' s = case s of
        LNil -> error "view past end of seq"
        LCons v _ -> v
        LLvl _ rest -> view' rest
        LTr {} -> view' (trim d s)

alterC :: a -> Zip a -> Zip a
alterC e (Zip l _ r) = Zip l e r

-- actually this is very similar to "view"
alter :: Dir -> a -> Zip a -> Zip a
alter d elm (Zip l e r) = case d of
    L -> Zip (alter' l) e r
    R -> Zip l e (alter' r)
  where
    -- arguments that don't change between function calls
    -- are removed
    alter' s = case s of
        LNil -> error "alter past end of seq"
        LCons _ rest -> LCons elm rest
        LLvl lv rest -> LLvl lv (alter' rest)
        LTr {} -> alter' (trim d s)

itemCount :: Tree a -> Int
itemCount t = case t of
    Nil -> 0
    Leaf {} -> 1
    Bin _ c _ _ -> c

focus :: Tree a -> Int -> Zip a
focus t p
    | p >= c || p < 0 = error "out of bounds"
    | otherwise = focus' t p LNil LNil
  where
    c = itemCount t
    focus' t' p' l r = case t' of
        Nil -> error "internal Nil"
        Leaf elm -> if p' == 0
                      then Zip l elm r
                      else error "assertion failed"
        Bin lv _ bl br ->
            let c' = itemCount bl
            in if p' < c'
                 then focus' bl p' l (LLvl lv (LTr br r))
                 else focus' br (p'-c') (LLvl lv (LTr bl l)) r

-- should be an order-preserving append over Tree structures
append :: Tree a -> Tree a -> Tree a
append t1 t2 = case (t1,t2) of
    (Nil,_) -> t2
    (_,Nil) -> t1
    (Leaf _, Leaf _) -> error "full trees shouldn't be appended"
    (Leaf _, Bin lv _ l r) -> Bin lv tot (append t1 l) r
    (Bin lv _ l r, Leaf _) -> Bin lv tot l (append r t2)
    (Bin lv1 _ t1l t1r, Bin lv2 _ t2l t2r) ->
        if lv1 >= lv2
          then Bin lv1 tot t1l (append t1r t2)
          else Bin lv2 tot (append t1 t2l) t2r
  where
    tot = itemCount t1 + itemCount t2
