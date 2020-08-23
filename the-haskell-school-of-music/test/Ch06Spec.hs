module Ch06Spec where

import Ch06
import Data.List
import Euterpea
import Test.Hspec

spec :: Spec
spec = do
  describe "properRow" $
    specify "examples" $ do
      properRow (line melProper) `shouldBe` True
      -- no matter of ordering.
      properRow (line (reverse melProper)) `shouldBe` True
      -- rests should be ignored.
      properRow (line (intersperse (rest en) melProper)) `shouldBe` True
      properRow
        (line $
           fmap
             (note en . pitch)
             (take 12 [20 ..]))
        `shouldBe` True
      properRow (line (drop 1 melProper)) `shouldBe` False
      properRow (line $ head melProper : melProper) `shouldBe` False
  describe "retroPitches" $
    specify "examples" $
      retroPitches (line [c 4 en, d 4 qn])
        `shouldBe` line [d 4 en, c 4 qn]

  describe "toIntervals" $
    specify "examples" $
      toIntervals [1 :: Int, 5, 3, 6, 5, 0, 1, 1]
        `shouldBe` [ [1, 5, 3, 6, 5, 0, 1, 1]
                   , [4, -2, 3, -1, -5, 1, 0]
                   , [-6, 5, -4, -4, 6, -1]
                   , [11, -9, 0, 10, -7]
                   , [-20, 9, 10, -17]
                   , [29, 1, -27]
                   , [-28, -28]
                   , [0]
                   ]

  describe "getHeads" $
    specify "examples" $
      getHeads [[1 :: Int], [2, 3], [4, 5, 6], [7]] `shouldBe` [1, 2, 4, 7]

  describe "intervalClosure" $
    specify "examples" $
      intervalClosure [1 :: Int, 5, 3, 6, 5, 0, 1, 1]
        `shouldBe` [0, -28, 29, -20, 11, -6, 4, 1]
