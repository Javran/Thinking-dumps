module Day2Easy

import Data.Vect

Matrix : (m : Nat) -> (n : Nat) -> (a : Type) -> Type
Matrix m n a = Vect m (Vect n a)
