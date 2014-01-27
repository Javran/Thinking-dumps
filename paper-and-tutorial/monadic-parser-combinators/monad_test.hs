import Prelude hiding (Maybe,Just,Nothing)
import Control.Monad

import MPC.Monad
import Test.HUnit

-- plus n if it has something inside
maybePlus :: Int -> Int -> Maybe Int
maybePlus n x = Just $ x + n

-- fail if it is not less than n
maybeLessThan :: Int -> Int -> Maybe Int
maybeLessThan n x = if x < n then Just x else Nothing

-- 1 + 3 + 10 = 14
testR = Just 1 >>= maybePlus 3 >>= maybePlus 10

tests = TestList
    [ TestCase
        (assertEqual "should return 14"
            testR
            (Just 14))
    , TestCase
        (assertEqual "should fail"
            (testR >>= maybeLessThan 10 >>= maybePlus 1 >>= maybePlus 2)
            Nothing)
    ]

main = runTestTT tests
