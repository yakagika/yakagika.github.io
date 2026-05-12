-- | fp3.md 「数値型」節の練習問題.
module Fp3.NumericPracticeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp3.NumericPractice" $ do
  it "40 div 7 == 5 (飴40個を7人で分けたときの1人分)" $
    40 `div` 7 `shouldBe` (5 :: Int)
  it "40 mod 7 == 5 (余り)" $
    40 `mod` 7 `shouldBe` (5 :: Int)
  it "底辺5×高さ4の三角形の面積 == 10.0" $
    (5 * 4 / 2 :: Double) `shouldBe` 10.0
  it "2^8 == 256" $
    (2 ^ (8 :: Int) :: Int) `shouldBe` 256
  it "2 * 3^2 == 18 (^ が * より優先)" $
    (2 * 3 ^ (2 :: Int) :: Int) `shouldBe` 18
  it "(2 * 3)^2 == 36" $
    ((2 * 3) ^ (2 :: Int) :: Int) `shouldBe` 36
