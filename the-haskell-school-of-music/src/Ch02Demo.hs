module Ch02Demo where

import Euterpea

{-
  Music theory concepts:

  - Octave: the interval between one musical pitch and another with double its frequency.
  - Pitch: perceptual, allows sounds' ordering on a frequency-related scale.
  - Pitch Class: "The pitch class C stands for all possible Cs, in whatever octave position."
  - Note: Pitch + Duration
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
     TODO: through trial and error we know the half steps to go:

     (D,4) = trans 2 (C,4)
     (G,4) = trans 7 (C,4)

     but still I don't understand where does these "2" and "7" come from?
     -}
    dMinor = pal $ note dz <$> minorChord (trans 2 pz)
    gMajor = pal $ note dz <$> majorChord (trans 7 pz)
    cMajor = pal $ note ddz <$> majorChord pz
