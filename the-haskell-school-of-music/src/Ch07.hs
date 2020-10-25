module Ch07 where

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
