{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ch06 where

import Ch04 (mel1)
import qualified Data.Set as S
import Euterpea

{-
  Ex 6.1:

  - all makes intuitive sense, not gonna carry out full proof,
    but can make some notes regarding them:

  - note that `line . lineToList == id` and `lineToList . line == id`,
    expanding `retro . retro` will make some line vs lineToList cancellings.

  - from same principle, expanding `retroInvert . invertRetro`
    will get everything cancelled out to get `retroInvert . invertRetro = id`

 -}

-- traverse a Music structure, by a function on Primitive, and concatenate results.
-- it is assumed that this Music is composed from a `line` function and does not consist of (:=:).
traverseLined :: (Primitive a -> [b]) -> Music a -> [b]
traverseLined f mp = case mp of
  Prim prim -> f prim
  Modify _ m -> traverseLined f m
  m0 :+: m1 -> traverseLined f m0 <> traverseLined f m1
  _ :=: _ ->
    -- I believe in some cases it is not impossible to implement this for :=: case,
    -- but I can imagine it being complicated and exercise does not require us to implement this anyway.
    []

melProper :: [Music Pitch]
melProper = fmap (\p -> note sn (p, 4)) [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

{- Ex 6.2 -}
properRow :: Music Pitch -> Bool
properRow = checkProper . traverseLined extractPc . removeZeros
  where
    -- this list is obtained from definition of `pitch` function:
    properPitchSet = S.fromList [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    extractPc prim =
      case prim of
        Note d (pc, _) ->
          -- rest are allowed, therefore we should ignore notes that have no time length.
          [pc | d /= 0]
        Rest _ -> []

    -- Note that simply checking whether we have exactly 12 notes is not quite right.
    -- check https://en.wikipedia.org/wiki/Pitch_class for details.
    checkProper xs =
      -- has exactly 12 notes
      length xs == S.size properPitchSet
        &&
        -- has the same set of pitches.
        curPitchSet == properPitchSet
      where
        curPitchSet = S.fromList xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = and $ zipWith (==) (take half xs) (reverse xs)
  where
    half = l `quot` 2
    l = length xs

{- Ex 6.3 -}
palin :: Music Pitch -> Bool
palin = isPalindrome . traverseLined extractNote . removeZeros
  where
    extractNote prim = case prim of
      Note _ (pc, _) -> [pc]
      Rest {} -> []

{- not really useful for now, but an isomorphism can be observed here. -}
unpairPrim :: Primitive a -> (Dur, Maybe a)
unpairPrim = \case
  Note d v -> (d, Just v)
  Rest d -> (d, Nothing)

pairPrim :: (Dur, Maybe a) -> Primitive a
pairPrim (d, m) = case m of
  Nothing -> Rest d
  Just v -> Note d v

{- Ex 6.4 -}
retroPitches :: Music Pitch -> Music Pitch
retroPitches =
  -- note that this implementation does not try to reconstruct the original structure and
  -- does not work on Music that has :=: in it. But for the use case this is simple and good enough.
  line
    . uncurry (zipWith (\a b -> Prim $ pairPrim (a, b)))
    . (\(xs, ys) -> (xs, reverse ys))
    . unzip
    . traverseLined (pure . unpairPrim)
    -- line function might add a zero duration rest in the end so
    -- we want to remove it by dropping pitches that has non-positive durations.
    . removeZeros

ex6_5 :: Music Pitch
ex6_5 = cut 2 (m /=: m)
  where
    m = forever (line mel1)

{-
  Reference: https://en.wikipedia.org/wiki/Ornament_(music)
 -}
mordent :: Int -> Music Pitch -> Music Pitch
mordent dp = transNote
  where
    transNote :: Music Pitch -> Music Pitch
    transNote (Prim (Note t p)) = line [note (t / 4) p, note (t / 4) (trans dp p), note (t / 2) p]
    transNote (Modify t m) = Modify t (transNote m)
    transNote _ = error "Expect single note."

upperMordent :: Music Pitch -> Music Pitch
upperMordent = mordent 1

lowerMordent :: Music Pitch -> Music Pitch
lowerMordent = mordent (-1)

turn :: Music Pitch -> Music Pitch
turn (Prim (Note t p)) = line $ fmap q [trans 1 p, p, trans (-1) p, p]
  where
    q = note (t/4)
turn (Modify t m) = Modify t (turn m)
turn _ = error "Expect single note."
