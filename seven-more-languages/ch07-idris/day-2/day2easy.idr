module Day2Easy

import Data.Vect

-- write a type for an m x n matrix
Matrix : (m : Nat) -> (n : Nat) -> (a : Type) -> Type
Matrix m n a = Vect m (Vect n a)

mat_eye3 : Matrix 2 2 Integer
mat_eye3 = [[1,0],[0,1]]

mat2 : Matrix 3 2 Char
mat2 = [['a','b'], ['c','d'], ['e','f']]

{-
reverseVec : Vect n a -> Vect n a
reverseVec xs = reverseVecAux [] xs
  where
    reverseVecAux : Vect x a -> Vect y a -> Vect (x+y) a
    reverseVecAux xs ys with (length ys)
      reverseVecAux xs [] | Z = xs
      reverseVevAux xs (y:ys1) | (S k) = reverseVecAux (y:xs) ys1
-}
