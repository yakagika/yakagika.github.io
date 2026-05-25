-- | fp5.md 「パターンマッチ」節の `fib` (パターンマッチ版).
module Fp5.FibPatternSpec (spec) where

import Test.Hspec

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

spec :: Spec
spec = describe "Fp5.FibPattern" $ do
  it "fib 0 == 1" $ fib 0 `shouldBe` 1
  it "fib 1 == 1" $ fib 1 `shouldBe` 1
  it "fib 5 == 8" $ fib 5 `shouldBe` 8
  it "fib 10 == 89" $ fib 10 `shouldBe` 89
