module ComplexNumbers
  ( Complex
  , conjugate
  , abs
  , exp
  , real
  , imaginary
  , mul
  , add
  , sub
  , div
  , complex
  )
where

import Prelude hiding (abs, div, exp)
import qualified Prelude

data Complex a = a :+ a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry (:+)

conjugate :: Num a => Complex a -> Complex a
conjugate (re :+ im) = re :+ (- im)

abs :: Floating a => Complex a -> a
abs (re :+ im) = sqrt $ re * re + im * im

real :: Num a => Complex a -> a
real (re :+ _) = re

imaginary :: Num a => Complex a -> a
imaginary (_ :+ im) = im

exp :: Floating a => Complex a -> Complex a
exp (a :+ b) = (expA * cos b) :+ (expA * sin b)
  where
    expA = Prelude.exp a

mul :: Num a => Complex a -> Complex a -> Complex a
mul (a :+ b) (c :+ d) = (a * c - b * d) :+ (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (a :+ b) (c :+ d) = (a + c) :+ (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (a :+ b) (c :+ d) = (a - c) :+ (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (a :+ b) (c :+ d) =
  ((a * c + b * d) / s) :+ ((b * c - a * d) / s)
  where
    s = c * c + d * d
