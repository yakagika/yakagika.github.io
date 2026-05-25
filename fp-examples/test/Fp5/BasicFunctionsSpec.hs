-- | fp5.md 「関数と演算子」節の基礎例と練習問題.
module Fp5.BasicFunctionsSpec (spec) where

import Test.Hspec

-- 基礎例
f :: Int -> Int
f x = x + 1

multiple :: Double -> Double -> Double
multiple x y = x * y

(.*) :: Double -> Double -> Double
x .* y = x * y

-- 練習問題
double :: Int -> Int
double x = 2 * x

g :: Int -> Int -> Int
g x y = x * y + x + y

(.+) :: Int -> Int -> Int
x .+ y = x + y + 1

divide :: Double -> Double -> Double
divide x y = x / y

spec :: Spec
spec = describe "Fp5.BasicFunctions" $ do
  it "f 4 == 5" $ f 4 `shouldBe` 5
  it "multiple 3 4 == 12.0" $ multiple 3 4 `shouldBe` 12.0
  it "3 .* 4 == 12.0" $ 3 .* 4 `shouldBe` 12.0

  it "double 7 == 14" $ double 7 `shouldBe` 14
  it "g 2 3 == 11" $ g 2 3 `shouldBe` 11
  it "4 .+ 5 == 10" $ 4 .+ 5 `shouldBe` 10
  it "10 `divide` 3 == 10/3" $
    10 `divide` 3 `shouldBe` (10 / 3 :: Double)
