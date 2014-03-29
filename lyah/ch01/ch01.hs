-- purely functional
-- lazy
-- statically typed
-- type system that has type inference

doubleMe = (*2)

main = print $ map (doubleMe.doubleMe.doubleMe) xs
    where xs = [1..9]
