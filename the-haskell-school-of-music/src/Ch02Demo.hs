{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch02Demo where

import Euterpea

{-
  Music theory concepts:

  - Octave: the interval between one musical pitch and another with double its frequency.
  - Pitch: perceptual, allows sounds' ordering on a frequency-related scale.
  - Pitch Class: "The pitch class C stands for all possible Cs, in whatever octave position."
  - Note: Pitch + Duration
  - Triad: a set of three notes called the "root", the "third", the "fifth" from
    lowest to highest pitch.
    + the third can be a "minor third" (three semitones) or a "major third" (four semitones)
 -}

t251 :: Music Pitch
t251 = dMinor :+: gMajor :+: cMajor
  where
    dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
    gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
    cMajor = c 4 bn :=: e 4 bn :=: g 4 bn

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pz dz = dMinor :+: gMajor :+: cMajor
  where
    ddz = 2 * dz
    -- A minor triad can be represented by the integer notation {0, 3, 7}.
    minorChord p = [p, trans 3 p, trans 7 p]
    -- A major triad is represented by the integer notation {0, 4, 7}.
    majorChord p = [p, trans 4 p, trans 7 p]
    pal = foldr1 (:=:)
    {-
      through trial and error we know the half steps to go:
      (actually it's just looking at `take _ $ iterate (trans 1) (C,4)`)

       (D,4) = trans 2 (C,4)
       (G,4) = trans 7 (C,4)

      for the magic number 2,7, consider 12-tone equal temperament scale:

        0: C, 1: C#, 2: D, 3: D#, 4: E,
        5: F, 6: F#, 7: G, 8: G#, 9: A, 10: A#, 11: B

      there we go.
     -}
    dMinor = pal $ note dz <$> minorChord (trans 2 pz)
    gMajor = pal $ note dz <$> majorChord (trans 7 pz)
    cMajor = pal $ note ddz <$> majorChord pz

data BluesPitchClass = Ro | MT | Fo | Fi | MS
type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Functor f => f BluesPitch -> f Pitch
fromBlues = fmap convert
  where
    convert (pc,o) = (,o) $ case pc of
      Ro -> C
      MT -> Ef
      Fo -> F
      Fi -> G
      MS -> Bf
