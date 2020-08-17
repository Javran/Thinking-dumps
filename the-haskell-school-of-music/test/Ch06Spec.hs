module Ch06Spec where

import Ch06
import Euterpea
import Test.Hspec
import Data.List

spec :: Spec
spec = do
  describe "properRow" $
    specify "examples" $ do
      properRow (line melProper) `shouldBe` True
      properRow (line (reverse melProper)) `shouldBe` True
      properRow (line (intersperse (rest en) melProper)) `shouldBe` True
  describe "retroPitches" $
    specify "examples" $
      retroPitches (line [c 4 en, d 4 qn])
        `shouldBe` line [d 4 en, c 4 qn]
