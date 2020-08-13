module Ch06Spec where

import Ch06
import Euterpea
import Test.Hspec

spec :: Spec
spec =
  describe "retroPitches" $
    specify "examples" $
      retroPitches (line [c 4 en, d 4 qn])
        `shouldBe` line [d 4 en, c 4 qn]
