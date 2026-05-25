-- | fp5.md 「case式」節の `fib` (case 版).
module Fp5.FibCaseSpec (spec) where

import Test.Hspec

fib :: Int -> Int
fib n = case n of
  0 -> 1
  1 -> 1
  _ -> fib (n - 1) + fib (n - 2)

return10 :: a -> Int
return10 _ = 10

spec :: Spec
spec = describe "Fp5.FibCase" $ do
  it "fib 5 == 8" $ fib 5 `shouldBe` 8
  it "fib 10 == 89" $ fib 10 `shouldBe` 89
  it "return10 \"anything\" == 10" $ return10 "anything" `shouldBe` 10
  it "return10 42 == 10" $ return10 (42 :: Int) `shouldBe` 10
