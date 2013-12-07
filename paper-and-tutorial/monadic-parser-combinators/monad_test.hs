import Control.Monad

import MPC.Monad

-- plus n if it has something inside
maybePlus :: Int -> Int -> Maybe1 Int
maybePlus n x = Just1 $ x + n

-- fail if it is not less than n
maybeLessThan :: Int -> Int -> Maybe1 Int
maybeLessThan n x = if x < n then Just1 x else Nothing1

main = do
    -- 1 + 3 + 10 = 14
    let r1 = return 1 >>= maybePlus 3 >>= maybePlus 10 
    -- fail
    let r2 = r1 >>= maybeLessThan 10 >>= maybePlus 1 >>= maybePlus 2
    print $ (r1,r2)
