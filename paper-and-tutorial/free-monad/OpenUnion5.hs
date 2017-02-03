{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}
module OpenUnion5 where

-- http://okmij.org/ftp/Haskell/extensible/OpenUnion5.hs

import Unsafe.Coerce (unsafeCoerce)

-- "r" is thought as an universe and "Int" value being the index of "t"
-- (not sure?) basically we can hide "t" behind the universe and use "Union r" in place of it
data Union (r :: [* -> *]) v where
    Union :: !Int -> t v -> Union r v

inj' :: Int -> t v -> Union r v
inj' = Union

-- here, are we using the index to establish type equivalence?
prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x)
    | n == n' = Just (unsafeCoerce x)
    | otherwise = Nothing
