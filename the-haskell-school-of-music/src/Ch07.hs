module Ch07 where

import Data.Function

{-
  ex 7.1.

  the proofs of Eq and Ord law are too boring
  as those are simply structural based proofs.

  so I'll just make few notes instead:

  - Note that we are dealing with `Eq a => Eq (Music a)`, rather
    than `Eq (Music a)` (which might not be possible), so
    at some point in the proof, `==` on Music will be calling `==` on type `a`
    therefore relying on `Eq a` being an valid instance in the first place.

  - Note that we are not just dealing with `Music`, we are also dealing with
    `Primitive`, `Control` (which will need to deal with `InstrumentName`, `PhraseAttribute` etc.),
   to fully prove those laws will require fully proving
   that their `Ord` instance satisfy the law.

 -}

{-
  ADT for ex 7.2
 -}
data MyColor = MyRed | MyGreen | MyBlue

instance Eq MyColor where
  MyRed == MyRed = True
  MyGreen == MyGreen = True
  MyBlue == MyBlue = True
  _ == _ = False

instance Enum MyColor where
  fromEnum MyRed = 0
  fromEnum MyGreen = 1
  fromEnum MyBlue = 2

  toEnum 0 = MyRed
  toEnum 1 = MyGreen
  toEnum 2 = MyBlue
  toEnum _ = error "invalid number"

instance Ord MyColor where
  compare = compare `on` fromEnum
