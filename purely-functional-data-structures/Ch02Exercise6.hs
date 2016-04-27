module Ch02Exercise6 where

import Prelude hiding (lookup)
import Data.Maybe

import Test.Hspec
import Control.Monad
import Control.Monad.Random hiding (fromList)
import Data.Foldable

-- the only thing we need is just the data type definition
-- actually there are not much thing we can reuse from UnbalancedSet (a.k.a Ch02BST):
-- it's hard to implement UnbalancedSet in terms of FiniteMap
-- but the other way around isn't too hard.
import Ch02BST (BST(..),toAscList)
import qualified Data.IntMap as IM

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

type FiniteMap k v = BST (k,v)

-- "member" can be implemented using "lookup" but not the
-- other way around.
lookup :: Ord a => a -> FiniteMap a b -> Maybe b
lookup _ E = Nothing
lookup x (T l (k,v) r)
    | x < k = lookup x l
    | x > k = lookup x r
    | x == k = Just v
    | otherwise = error "lookup: impossible"

member :: Ord a => a -> FiniteMap a b -> Bool
member x = isJust . lookup x

-- we cannot reuse "insert" from BST
-- simply because now every element is not just a key
-- but contains some extra data (value)
-- if we are inserting an existing key into this finite map
-- we will usually expect this new insertion to replace the old key-value pair
-- which does not happen in BST's implementation
insert, bind :: Ord a => a -> b -> FiniteMap a b -> FiniteMap a b

insert k v E = T E (k,v) E
insert k v (T l e@(curK,_) r)
    | k < curK = T (insert k v l) e r
    | k > curK = T l e (insert k v r)
    | k == curK = T l (k,v) r
    | otherwise = error "insert: impossible"

bind = insert

empty :: FiniteMap a b
empty = E

fromList :: Ord a => [(a,b)] -> FiniteMap a b
fromList = foldl' (\acc (k,v) -> insert k v acc) E

main :: IO ()
main = hspec $ do
    describe "FiniteMap" $ do
        let genRandomInts count = replicateM count (getRandomR (0 :: Int,200))
            testOnScale n = do
                let desc = "should have the same results as IntMap when keys are Ints "
                        ++ "(n = " ++ show n ++ ")"
                it desc $ do
                    pairs <- zip <$> genRandomInts n <*> genRandomInts n
                    -- step1: test insertion
                    let im = IM.fromList pairs
                        result1 = IM.toAscList im
                        fm = fromList pairs
                        result2 = toAscList fm
                    result2 `shouldBe` result1
                    -- step2: test membership (dictionary lookup)
                    let searchResults1 = map (\k -> IM.lookup k im) [0..200]
                        searchResults2 = map (\k -> lookup k fm) [0..200]
                    searchResults2 `shouldBe` searchResults1
        -- intentionally insert an insufficient amount of pairs
        -- so we can get some coverage from key-missing cases
        testOnScale 150
        testOnScale 10000
