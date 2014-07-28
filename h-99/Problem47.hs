module Problem47 where

import Prelude hiding (and,or)
import qualified Problem46 as P (and,or)
import Problem46 (table)

and, or :: Bool -> Bool -> Bool
and = P.and
or = P.or

infix 3 `and`
infix 2 `or`

main :: IO ()
main = table (\a b -> a `and` (a `or` not b))
