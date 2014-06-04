{-
  "where" bindings are one way to potentially simplify your code a lot
    usually "where" appears after the expression it binds to
-}


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat    = "fat"
    | otherwise     = "whale"
    where bmi = weight / height ^ (2 :: Int)
          (skinny,normal,fat) = (18.5,25.0,30.0)

main :: IO ()
main = undefined
