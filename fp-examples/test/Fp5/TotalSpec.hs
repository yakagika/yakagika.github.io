-- | lectures/fp/fp5.md 「再帰」節の `total` サンプル.
module Fp5.TotalSpec (spec) where

import Test.Hspec

total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

spec :: Spec
spec = describe "Fp5.Total" $ do
  it "total [] == 0" $
    total [] `shouldBe` 0
  it "total [1,2,3] == 6" $
    total [1, 2, 3] `shouldBe` 6
  it "total [1..10] == 55 (fp5.md の期待値)" $
    total [1 .. 10] `shouldBe` 55
