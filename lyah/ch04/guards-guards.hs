{-
  guards tests if some properties are met
  before proceed to evaluate the following expression
  and the properties (predicates) are checked in order
  which means that guards will keep a function being a function
  (or otherwise it will be a relation (like in Prolog) which
  can return multiple results for one input value)
-}
bmiTell :: (RealFrac a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise   = "whale"

-- use guard to write "max'" function
max' :: Ord a => a -> a -> a
max' a b
     | a > b     = a
     | otherwise = b

main :: IO ()
main = do
    print $ bmiTell (23.4 :: Float)
    print $ bmiTell (14.0 :: Float)
    print $ max' 'a' 'b'
