import Prelude hiding (Maybe,Just,Nothing)
import Control.Monad

import MPC.Monad
import Test.HUnit (Test(..), assertEqual, runTestTT)

-- plus n if it has something inside
maybePlus :: Int -> Int -> Maybe Int
maybePlus n x = Just $ x + n

-- fail if it is not less than n
maybeLessThan :: Int -> Int -> Maybe Int
maybeLessThan n x = if x < n then Just x else Nothing

exceptionMonadTests = TestList
    [ TestCase
        (assertEqual "should return 14"
            testR
            (Just 14))
    , TestCase
        (assertEqual "should fail"
            (testR >>= maybeLessThan 10 >>= maybePlus 1 >>= maybePlus 2)
            Nothing)
    ]
    where
        -- 1 + 3 + 10 = 14
        testR = Just 1 >>= maybePlus 3 >>= maybePlus 10

nondetMonadTests = TestList
    [ TestCase
        (assertEqual "half way of Pascal's triangle"
            ([1]                 >>= split1 >>= split1 >>= split1)
            (toList (Lst 1 Empty >>= split2 >>= split2 >>= split2)))
    , TestCase
        (assertEqual "test guard" 
            (do
                x <- [1..20]
                guard $ odd x
                return x)
            (toList 
                (do x <- fromList [1..20]
                    guard $ odd x
                    return x)))
    ]
    where
        split1 x = [x, x+1]
        split2 x = Lst x (Lst (x+1) Empty)

-- make a stack and see how it works
pop :: State [a] a
pop = State $ \(x:xs) -> (x,xs)

push :: a -> State [a] ()
push v = State $ \xs -> ((), v:xs)

stateMonadTests = TestList
    [ TestCase
        (assertEqual "simulate a stack"
            (runState pop [1])
            (1,[]))
    , TestCase
        (assertEqual "more complex example"
            (runState m [])
            ((),[9,3,1]))
    ]
    where
        m = do
            push 1
            push 2
            push 4
            pop
            pop
            push 3
            push 9

main = mapM_ runTestTT
    [ exceptionMonadTests
    , nondetMonadTests
    , stateMonadTests
    ]
