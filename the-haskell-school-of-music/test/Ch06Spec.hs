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
