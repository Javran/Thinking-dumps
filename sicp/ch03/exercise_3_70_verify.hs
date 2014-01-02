import Data.Function (on)
import Data.List (sortBy)
import Control.Monad (guard)

divisible x y = x `mod` y == 0

notDivisibleBy _ [] = True
notDivisibleBy x (y:ys) = not (divisible x y) && notDivisibleBy x ys

theResult = do
    -- cover all possibilities
    x <- [1..100]
    y <- [x..100]
    -- filter out invalid i,j
    guard $ notDivisibleBy x [2,3,5]
    guard $ notDivisibleBy y [2,3,5]
    return (x,y)

weight (i,j) = 2*i + 3*j + 5*i*j

-- sort by weight
main = print $ take 20 $ sortBy (compare `on` weight) theResult
