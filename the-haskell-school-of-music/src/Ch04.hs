{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch04 where

import Data.List (inits)
import Euterpea hiding (times)

twinkle :: Music Pitch
twinkle = line1 [pat0, pat1, pat1, pat0]
  where
    pat0 = mkLine [C,C,G,G,A,A,G] :+: mkLine [F,F,E,E,D,D,C]
    pat1 = mkLine [G,G,F,F,E,E,D]
    mkLine =
      line1
      . zipWith (\d pc -> note d (pc,4)) (replicate 6 qn <> [hn])

times :: Int -> Music a -> Music a
times n m
  | n < 0 = error "expected non-negative input"
  | n == 0 = rest 0
  | n == 1 = m
  | otherwise = case n `quotRem` 2 of
      (n',0) -> let half = times n' m in half :+: half
      (n',1) -> let half = times n' m in m :+: half :+: half
      _ -> error "unreachable"

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d = line . map ($ d)

bassLine :: Music Pitch
bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
  where
    b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
    b2 = addDur dqn [b 3, es 4, fs 4, es 4]
    b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

{-
  transforms a primitive note into a specific interpretation of grace note.
 -}
graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) =
  note (d/8) (trans n p) :+: note (7 * d/8) p
graceNote _ _ = error "non-primitives are not supported"

mainVoice :: Music Pitch
mainVoice = times 3 v1 :+: v2
  where
    v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b
    v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
    v1b = addDur en [cs 5, b 4]

    v2 = v2a :+: v2b :+: v2c :+: v2e :+: v2f :+: v2g
    v2a = line [cs 5 (dhn + dhn), d 5 dhn, f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en]
    v2b =
      addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn
      :+: addDur en [as 4, cs 5, fs 5, e 5, fs 5]
    v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en]
      :+: e 5 en :+: enr :+: line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]
    v2e = line [ graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en
               , graceNote 1 (cs 5 qn), b 4 (en + hn), cs 5 en, b 4 en
               ]
    v2f = line [ fs 5 en, a 5 en, b 5 (hn+qn), a 5 en, fs 5 en, e 5 qn
               , d 5 en, fs 5 en, e 5 hn, b 4 (en + hn), cs 5 en, b 4 en
               ]
    v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+: b 4 (3 * dhn + hn)

childSong6 :: Music Pitch
childSong6 = instrument RhodesPiano (tempo t (bassLine :=: mainVoice))
  where
    t = (dhn / qn) * (69 / 120)

prefixes :: [a] -> [[a]]
prefixes =
  -- This should be equivalent to the original definition.
  tail . inits

prefix :: [Music a] -> Music a
prefix mel = m :+: transpose 5 m :+: m
  where
    m1 = line (concat (prefixes mel))
    m2 = transpose 12 (line (concat (prefixes (reverse mel))))
    m = instrument Flute m1 :=: instrument VoiceOohs m2

mel1, mel2 :: [Music Pitch]
mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]

{-
  Ex 4.1

  This is a simplified version of National Anthem of Russia transcribed from:
  https://toplayalong.com/sheet-music/national-anthem-of-russia-recorder/

 -}
russia :: Music Pitch
russia =
    chunk0 :+: chunk1 :+: chunk2 :+: chunk3 :+: chunk4fst
    :+: chunk0sub :+: chunk1 :+: chunk2 :+: chunk3 :+: chunk4snd
  where
    chunk0 =
      g 4 en :+: chunk0sub
    chunk0sub =
      c 5 qn :+: pat0 (g 4) (a 4) :+: b 4 qn :+: e 4 en :+: e 4 en
      :+: a 4 qn :+: pat0 (g 4) (f 4) :+: g 4 qn :+: c 4 en :+: c 4 en
      :+: d 4 qn :+: d 4 en :+: e 4 en :+: f 4 qn :+: f 4 en :+: g 4 en
      :+: a 4 qn :+: b 4 en :+: c 5 en :+: d 5 hn
    chunk1 =
      -- 5
      e 5 qn :+: pat0 (d 5) (c 5) :+: d 5 qn :+: b 4 en :+: g 4 en
      :+: c 5 qn :+: pat0 (b 4) (a 4) :+: b 4 qn :+: e 4 en :+: e 4 en
      :+: a 4 qn :+: pat0 (g 4) (f 4) :+: g 4 qn :+: pat0 (c 4) (c 4)
      :+: c 5 qn :+: pat0 (b 4) (a 4) :+: g 4 en :+: b 4 en :+: c 5 en :+: d 5 en
    chunk2 =
      -- 9
      e 5 hn :+: d 5 en :+: c 5 en :+: b 4 en :+: c 5 en
      :+: d 5 (qn + en) :+: g 4 en :+: g 4 en :+: b 4 en :+: c 5 en :+: d 5 en
      :+: c 5 hn :+: b 4 en :+: a 4 en :+: g 4 en :+: a 4 en
      :+: b 4 (qn + en) :+: e 4 qn :+: e 4 en :+: g 4 en :+: a 4 en :+: b 4 en
      :+: let r = c 5 qn :+: pat0 (a 4) (b 4) in r :+: r
    chunk3 =
      -- 14
      c 5 qn :+: a 4 en :+: c 5 en :+: f 5 hn
      :+: f 5 hn :+: e 5 en :+: d 5 en :+: c 5 en :+: d 5 en
      :+: e 5 (qn + en) :+: c 5 en :+: c 5 hn
      :+: d 5 hn :+: c 5 en :+: b 4 en :+: a 4 en :+: b 4 en
    chunk4a =
      c 5 (qn + en) :+: a 4 en :+: a 4 hn
      :+: c 5 qn :+: pat0 (b 4) (a 4) :+: g 4 qn :+: c 4 qn
    chunk4fst =
      chunk4a
      :+: c 5 qn :+: pat0 (b 4) (a 4) :+: g 4 (qn + en) :+: g 4 en
    chunk4snd =
      chunk4a
      :+: g 4 qn :+: a 4 en :+: b 4 en :+: c 5 hn
    -- a common pattern
    pat0 x y = x (en + sn) :+: y sn

russia' :: Music Pitch
russia' =
  foldl1 (:=:) $ fmap (\i -> instrument i russia) [AcousticGrandPiano, Violin, Bagpipe]

-- ex 4.2, the original melody is too long so here we only use a part of it.
myMel :: [Music Pitch]
myMel =
  [ g 4 en, c 5 qn, g 4 (en + sn), a 4 sn , b 4 qn , e 4 en , e 4 en
  , a 4 qn , g 4 (en + sn), f 4 sn , g 4 qn , c 4 en , c 4 en
  , d 4 qn , d 4 en , e 4 en , f 4 qn , f 4 en , g 4 en , a 4 qn , b 4 en , c 5 en , d 5 hn
  ]

myMelPref :: Music Pitch
myMelPref = prefix myMel

{-
  ex 4.3: honestly I'll say this exercise is kinda boring without a clear instruction about what to do,
  and `prefix` is a very boring function that creates a long melody but doesn't really have anything intersting...
  so yeah let's just do some random things...
 -}
myMel2 :: Music Pitch
myMel2 = prefix' myMel
  where
    prefix' :: [Music a] -> Music a
    prefix' mel = m :=: transpose 5 m
      where
        m1 = line mel
        m = instrument Violin m1 :=: instrument Cello m1
