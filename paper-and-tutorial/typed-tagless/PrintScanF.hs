module PrintScanF where

import Prelude hiding ((^))
import Data.Char
import Control.Monad

-- http://okmij.org/ftp/tagless-final/course/PrintScanF.hs

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use fst" #-}
{-# ANN module "HLint: ignore Collapse lambdas" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

class FormattingSpec repr where
    lit :: String -> repr a a
    int :: repr a (Int -> a)
    char :: repr a (Char -> a)
    fpp :: PrinterParser b -> repr a (b -> a)
    (^) :: repr b c -> repr a b -> repr a c

infixl 5 ^

data PrinterParser a = PrinterParser
  (a -> String)
  (String -> Maybe (a,String))

{-

  "Pr" for printing

  - it might look weird but think "FPr a b" like "b -> a".
  - on the use site, "sprintf" is really just build up a function
    accordingly, and its type "FPr String b -> b" indicates
    that the resulting function must be of type "b".
    and calling the resulting function with proper values will give us "a" back
  - if you look at "FormattingSpec" carefully:
    - "a" in "repr a b" is just being passed around,
      allowing future continuation to return arbitrary type
    - on the other hand "b" builds up, which is "encoding" what we want

-}
newtype FPr a b = FPr ((String -> a) -> b)

instance FormattingSpec FPr where
    -- continuation passing style
    -- basically every implementation begin with "FPr $ \k ->"
    -- and it seems that we can kind of recover the original impl

    -- lit' str = str
    lit str = FPr $ \k -> k str
    -- int' x = show x
    int = FPr $ \k -> \x -> k (show x)
    -- char' c = [c] -- or just "show c"
    char = FPr $ \k -> \x -> k [x]
    -- (a') ^ (b') = a' ++ b'
    (FPr a) ^ (FPr b) = FPr $ \k -> a (\sa -> b (\sb -> k (sa ++ sb)))
    -- not mentioned in the paper, but the motivation is clear:
    -- if we know how to print or parse for a particular type,
    -- we can support that type by using "fpp".
    fpp (PrinterParser pr _) = FPr $ \k -> \x -> k (pr x)

sprintf :: FPr String b -> b
sprintf (FPr fmt) = fmt id

-- "Sc" for scanning
newtype FSc a b = FSc (String -> b -> Maybe (a,String))

instance FormattingSpec FSc where
    lit str = FSc $ \inp f -> do
        inp' <- prefix str inp
        pure (f, inp')
    int = FSc $ \inp f -> case span isDigit inp of
        ([],_) -> Nothing
        (raw,inp') -> Just (f (read raw), inp')
    char = FSc $ \inp f -> case inp of
        (c:inp') -> Just (f c, inp')
        "" -> Nothing
    (FSc a) ^ (FSc b) = FSc $ \inp f ->
        a inp f >>= \(vb,inp') -> b inp' vb
    fpp (PrinterParser _ pa) = FSc $ \inp f -> do
        (v,s) <- pa inp
        pure (f v, s)

prefix :: String -> String -> Maybe String
prefix [] str = Just str
prefix (pc:pr) (sc:sr) = guard (pc == sc) >> prefix pr sr
prefix _ _ = Nothing

-- remaining input string is dropped from the result.
sscanf :: String -> FSc a b -> b -> Maybe a
sscanf inp (FSc fmt) f = fst <$> fmt inp f

sscanf' :: String -> FSc a b -> b -> Maybe (a,String)
sscanf' inp (FSc fmt) f = fmt inp f

-- a trick of getting rid of monomorphism restriction
-- is to define "fmt3 ()" rather than "fmt3".
fmt3 :: FormattingSpec repr => repr a (Char -> Int -> a)
fmt3 = lit "The value of " ^ char ^ lit " is " ^ int

test1 :: Maybe (Int,Int)
test1 = sscanf "123,456" (int ^ lit "," ^ int) (,)
