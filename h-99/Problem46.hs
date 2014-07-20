module Problem46 where

import Prelude hiding (and,or)

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)

and, or, nand, nor, xor, impl, equ :: Bool -> Bool -> Bool

and  = (&&)
or   = (||)
nand = not .: and
nor  = not .: or
xor  = (/=)
equ  = (==)

impl False _ = True
impl True b = b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ (\(a,b,c) -> putStrLn
                           . unwords
                           . map show
                           $ [a,b,c])
              [ (a,b,f a b)
              | a <- [True, False]
              , b <- [True, False]
              ]

main :: IO ()
main = table (\a b -> (and a (or a b)))
