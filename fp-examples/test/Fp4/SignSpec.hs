-- | lectures/fp/fp4.md 「otherwise」節の `sign` サンプル.
module Fp4.SignSpec (spec) where

import Test.Hspec

sign :: Int -> String
sign n | n > 0     = "正"
       | n < 0     = "負"
       | otherwise = "零"

spec :: Spec
spec = describe "Fp4.Sign" $ do
  it "sign 5 == \"正\""     $ sign 5    `shouldBe` "正"
  it "sign (-3) == \"負\""  $ sign (-3) `shouldBe` "負"
  it "sign 0 == \"零\""     $ sign 0    `shouldBe` "零"
