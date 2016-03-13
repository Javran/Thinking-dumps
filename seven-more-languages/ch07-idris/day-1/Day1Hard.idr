module Day1Hard

listReverse : List a -> List a
listReverse [] = []
listReverse (x :: xs) = reverse xs ++ [x]

listReverse2 : List a -> List a
listReverse2 = listReverse2aux []
  where
    listReverse2aux : List a -> List a -> List a
    listReverse2aux acc [] = acc
    listReverse2aux acc (x :: xs) = listReverse2aux (x :: acc) xs
