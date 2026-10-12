module Fp7.GreaterPairsSpec (spec) where

import Test.Hspec

greaterPairs :: [(Int, Int)]
greaterPairs = [ (x, y) | x <- [1 .. 5], y <- [1 .. 5], x > y ]

spec :: Spec
spec = describe "Fp7.GreaterPairs (内包表記と数式の対応)" $ do
  it "x > y を満たす組は 10 個" $ length greaterPairs `shouldBe` 10
  it "先頭の組" $ take 3 greaterPairs `shouldBe` [(2, 1), (3, 1), (3, 2)]
  it "すべての組が x > y" $ all (\(x, y) -> x > y) greaterPairs `shouldBe` True
