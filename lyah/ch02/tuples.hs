import Control.Monad (guard)

main = do
    let foobar = ("Foo", "Bar")
    print foobar
    print $ fst foobar
    print $ snd foobar
    print $ zip [1..] [1,2,4,8]

    let rightTriangles =
            [ (a,b,c)
            | c <- [1..10]
            , b <- [1.. c]
            , a <- [1.. b]
            , a^2 + b^2 == c^2]
    print rightTriangles

    let rightTriangles' = do
        c <- [1..10]
        b <- [1.. c]
        a <- [1.. b]
        guard $ a^2 + b^2 == c^2
        return (a,b,c)

    print rightTriangles'
