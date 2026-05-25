-- | fp5.md 「if式」節の `fib` (if 版).
module Fp5.FibIfSpec (spec) where

import Test.Hspec

fib :: Int -> Int
fib n = if n == 0 then 1
        else if n == 1 then 1
        else fib (n - 1) + fib (n - 2)

spec :: Spec
spec = describe "Fp5.FibIf" $ do
  it "fib 5 == 8" $ fib 5 `shouldBe` 8
  it "fib 10 == 89" $ fib 10 `shouldBe` 89
