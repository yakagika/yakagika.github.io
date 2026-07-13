-- | fp8.md 「演算の道と順序の道が出会う」節 — min = ∧, max = ∨ (二重の合流点).
module Fp8.BoolMeetJoinSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp8.BoolMeetJoin (二重の合流点)" $ do
  it "min True False == True && False" $
    min True False `shouldBe` (True && False)
  it "max True False == True || False" $
    max True False `shouldBe` (True || False)
  it "全 4 通りで min = ∧ かつ max = ∨" $
    and [ min x y == (x && y) && max x y == (x || y)
        | x <- [False, True], y <- [False, True] ] `shouldBe` True
