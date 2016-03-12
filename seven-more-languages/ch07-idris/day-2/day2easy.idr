module Day2Easy

import Data.Vect

Matrix : (m : Nat) -> (n : Nat) -> (a : Type) -> Type
Matrix m n a = Vect m (Vect n a)

mat_eye3 : Matrix 2 2 Integer
mat_eye3 = [[1,0],[0,1]]

mat2 : Matrix 3 2 Char
mat2 = [['a','b'], ['c','d'], ['e','f']]
