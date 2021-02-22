{-# LANGUAGE LambdaCase #-}

module Yacht
  ( yacht
  , Category (..)
  )
where

import Data.List

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

{-
  Many of the patterns used below are based on the assumption that input is always of length 5.
  And because the length is very small, so are the # of possible shapes that we can enumerate.
 -}

yacht :: Category -> [Int] -> Int
yacht = \case
  Ones -> matchAndMul 1
  Twos -> matchAndMul 2
  Threes -> matchAndMul 3
  Fours -> matchAndMul 4
  Fives -> matchAndMul 5
  Sixes -> matchAndMul 6
  FullHouse -> withGroupped $ \s g -> case g of
    [[_, _], [_, _, _]] -> s
    _ -> 0
  FourOfAKind -> withGroupped $ \_s g -> case g of
    [_, [x, _, _, _]] -> x * 4
    [x : _] -> x * 4
    _ -> 0
  LittleStraight -> (\x -> if x == [1 .. 5] then 30 else 0) . sort
  BigStraight -> (\x -> if x == [2 .. 6] then 30 else 0) . sort
  Choice -> sum
  Yacht -> \case
    x : xs -> if all (== x) xs then 50 else 0
    [] -> 0
  where
    matchAndMul i = sum . concatMap (\x -> [i | x == i])
    -- `f` is called back with input's sum and transformed structure ready for pattern matching.
    withGroupped f xs =
      f (sum xs) . sortOn length . group . sort $ xs
