-- | fp8.md 演習 CH8-1 の回答例.
module Fp8.DirectionShowSpec (spec) where

import Test.Hspec

data Direction = North | East | South | West

instance Show Direction where
  show North = "北"
  show East  = "東"
  show South = "南"
  show West  = "西"

spec :: Spec
spec = describe "Fp8.DirectionShow" $ do
  it "show North == \"北\"" $ show North `shouldBe` "北"
  it "show East == \"東\""  $ show East  `shouldBe` "東"
  it "show South == \"南\"" $ show South `shouldBe` "南"
  it "show West == \"西\""  $ show West  `shouldBe` "西"
