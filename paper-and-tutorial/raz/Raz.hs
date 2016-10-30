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

view :: Dir -> Zip a -> a
view d (Zip l e r) = case d of
    L -> view' L l
    R -> view' R r
  where
    view' d' s = case s of
        LNil -> error "view past end of seq"
        LCons e _ -> e
        LLvl _ rest -> view' d' rest
        LTr {} -> view' d' (trim d' s)
