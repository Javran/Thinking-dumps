{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ch06 where

import Ch04 (mel1, twinkle)
import Data.List (unfoldr)
import Data.Maybe
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
  Ex 6.6: realize mordent, turn and appoggiatura

  Reference: https://en.wikipedia.org/wiki/Ornament_(music)
 -}
expectSingleNote :: (Dur -> Pitch -> Music Pitch) -> Music Pitch -> Music Pitch
expectSingleNote f = tr
  where
    tr (Prim (Note t p)) = f t p
    tr (Modify t m) = Modify t (tr m)
    tr _ = error "Expect single note."

mordent :: Int -> Music Pitch -> Music Pitch
mordent dp =
  expectSingleNote (\t p -> line [note (t / 4) p, note (t / 4) (trans dp p), note (t / 2) p])

upperMordent :: Music Pitch -> Music Pitch
upperMordent = mordent 1

lowerMordent :: Music Pitch -> Music Pitch
lowerMordent = mordent (-1)

turn :: Music Pitch -> Music Pitch
turn = expectSingleNote $
  \t p ->
    let q = note (t / 4)
     in line $ fmap q [trans 1 p, p, trans (-1) p, p]

-- the interpretation on this one is a bit ambiguous,
-- so here I'm going to take the liberty and say let's divide
-- the principle note by half and trans on first half.
appoggiatura :: Int -> Music Pitch -> Music Pitch
appoggiatura i = expectSingleNote $
  \t p ->
    let t' = t / 2
     in line [note t' (trans i p), note t' p]

ex6_7 :: Music Pitch
ex6_7 = line ((\ps -> perc ps en) <$> allPercSounds)
  where
    allPercSounds =
      -- I guess it is expected to use fromEnum, but this also works,
      -- and actually is simpler.
      [AcousticBassDrum .. OpenTriangle]

ex6_8 :: Music Pitch
ex6_8 = cut 4 $ forever drumBeatBase :=: twinkle
  where
    -- just some random drum beats
    drumBeatBase = line [p qn, p sn, p en, p sn, p en, p sn, p sn, p sn, p sn, p en]
    p = perc AcousticSnare

-- ex 6.9
scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
scaleVolume s = fmap (\(p, v) -> (p, round $ s * fromIntegral v))

-- ex 6.10
retro' :: Music a -> Music a
retro' =
  mFold
    -- Prims are kept as it is.
    Prim
    -- sequential constructor are reversed
    (flip (:+:))
    retroParallel
    -- controls are kept unchanged as well
    Modify
  where
    {-
      this is actually the tricky part.
      note that there is no recursive call in the function body
      this is because "sub-trees" are already folded when passing to this function
      (in other words, mFold does the recursive work for us)
      it is intentional that variables are called m1' and m2',
      just to be clear that those subtrees are folded results.

      correctness is partially verified by an example in GHCi:

      >  retro' childSong6 == retro childSong6
      True
     -}
    retroParallel m1' m2' =
      if d1 > d2
        then m1' :=: (rest (d1 - d2) :+: m2')
        else (rest (d2 - d1) :+: m1') :=: m2'
      where
        d1 = dur m1'
        d2 = dur m2'

{-
  ex 6.11

  for this `insideOut` to work we simply just need to switch two constructors around.
  see implementation below.

  (a) I'm not sure what does "non-trivial" mean, but I guess anything
      with a "composite" constructor is non-trivial, which excludes primitive-only values.

  Let's try some example:

  (vA :+: vB) :=: (vC :+: vD) ~ (vA :=: vB) :+: (vC :=: vD)

  note that if dur of all vA,vB,vC,vD is the same and vB == vC, we'll have:

  (vA :+: vB) :=: (vB :+: vD) ~ (vA :=: vB) :+: (vB :=: vD)

 -}
insideOut :: Music a -> Music a
insideOut = mFold Prim (:=:) (:+:) Modify

{-
  Create a Music value m out of 3 values (assumed to be of the same duration),
  that satisfies `insideOut m == m`
 -}
mkInsideOutId :: Music a -> Music a -> Music a -> Music a
mkInsideOutId vA vB vD = (vA :+: vB) :=: (vB :+: vD)

{-
  We are using two "layers" of mkInsideOutId here to give a bit more complexity.
 -}
ex6_11_a :: Music Pitch
ex6_11_a = mkInsideOutId vA vB vD
  where
    vA = mkInsideOutId (d 5 qn) (e 5 qn) (c 5 qn)
    vB = mkInsideOutId (b 4 qn) (c 5 qn) (a 4 qn)
    vD = mkInsideOutId (g 4 qn) (a 4 qn) (f 4 qn)

{-
  Instruction not clear, skipping 6.11 (b)
 -}

x1 :: Music Pitch
x1 = g 4 qn :=: (c 4 en :+: d 4 en :+: e 4 en)

x2 :: Music Pitch
x2 = g 4 qn :=: tempo (3 / 2) (c 4 en :+: d 4 en :+: e 4 en)

phase1, phase2, phase3 :: Music Pitch
[phase1, phase2, phase3] = (\v -> phaseIt v tk) <$> [1.5, 1.1, 1.01]
  where
    tk = times 4 twinkle
    phaseIt factor m = m :=: tempo factor m

rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep _ _ 0 _ = rest 0
rep f g n m = m :=: g (rep f g (n -1) (f m))

repRun, repCascade, repCascades, repFinal :: Music Pitch

repRun', repCascade', repCascades', repFinal' :: Music Pitch

repRun = rep (transpose 5) (offset tn) 8 (c 4 tn)

repCascade = rep (transpose 4) (offset en) 8 repRun

repCascades = rep id (offset sn) 2 repCascade

repFinal = repCascades :+: retro repCascades

repRun' = rep (offset tn) (transpose 5) 8 (c 4 tn)

repCascade' = rep (offset en) (transpose 4) 8 repRun'

repCascades' = rep (offset sn) id 2 repCascade'

repFinal' = repCascades' :+: retro repCascades'

-- ex 6.12 (a)
toIntervals :: Num a => [a] -> [] [a]
toIntervals = unfoldr go
  where
    go ys = do
      _ : ys' <- pure ys
      pure (ys, zipWith (-) ys' ys)

-- ex 6.12 (b)
getHeads :: [[a]] -> [a]
getHeads = mapMaybe get
  where
    -- it doesn't sound necessary to handle the empty case,
    -- so this case is best effort (as it is the input value that breaks the contract)
    get [] = Nothing
    get (x : _) = Just x

-- ex 6.12 (c)
intervalClosure :: Num a => [a] -> [a]
intervalClosure = reverse . getHeads . toIntervals

{-
  ex 6.12 (d)

  I'm not sure what this exercise is asking about,
  but it makes some sense to take the input list as "seed" ic0 and
  build intervalClosure ic1, then ic2, and so on.
  so let's do just that.
 -}
intervalClosures :: Num a => [a] -> [[a]]
intervalClosures = iterate intervalClosure

{-
  (WIP) ex 6.13

  test: cut _ shepardTone
 -}
shepardTone :: Music (Pitch, Volume)
shepardTone = line descendingPitches :=: (rest (hn * 4) :+: shepardTone)
  where
    len = 12 * 4
    descendingPitches = zipWith addVolume vols descendingNotes
    descendingNotes = fmap (note sn . pitch) (take len [60, 59 ..])
    fadingSteps = 10 -- steps spent in fading in or out.
    fadeInVol :: [Volume]
    fadeInVol =
      [ round $ fromIntegral (i * 127) / (10 :: Double)
      | i <- [1 :: Int .. fadingSteps]
      ]
    fadeOutVol = reverse fadeInVol
    vols = fadeInVol <> replicate (len - fadingSteps - fadingSteps) 127 <> fadeOutVol
