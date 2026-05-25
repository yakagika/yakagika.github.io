-- | lectures/fp/fp5.md 「ガード」節の `fib` (otherwise フォールバック付き).
module Fp5.FibSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

fib :: Int -> Int
fib n | n == 0    = 1
      | n == 1    = 1
      | n >= 2    = fib (n - 1) + fib (n - 2)
      | otherwise = error "fib: negative input"

spec :: Spec
spec = describe "Fp5.Fib" $ do
  it "fib 0 == 1" $ fib 0 `shouldBe` 1
  it "fib 1 == 1" $ fib 1 `shouldBe` 1
  it "fib 5 == 8 (fp5.md の期待値)" $ fib 5 `shouldBe` 8
  it "fib 10 == 89" $ fib 10 `shouldBe` 89
  it "fib (-1) は error を投げる" $
    evaluate (fib (-1)) `shouldThrow` errorCall "fib: negative input"
