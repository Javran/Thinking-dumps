module Day1Hard

listReverse : List a -> List a
listReverse [] = []
listReverse (x :: xs) = reverse xs ++ [x]

-- TODO: one with helper?
