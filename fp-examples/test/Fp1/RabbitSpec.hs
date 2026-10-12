module Fp1.RabbitSpec (spec) where

import Test.Hspec

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

lateRabbits :: Int -> Int
lateRabbits 0 = 1
lateRabbits 1 = 1
lateRabbits 2 = 1
lateRabbits n = lateRabbits (n - 1) + lateRabbits (n - 3)

spec :: Spec
spec = describe "Fp1.Rabbit" $ do
  it "fib 12 == 233 (12ヶ月目)" $ fib 12 `shouldBe` 233
  it "fib 11 == 144 (11ヶ月目)" $ fib 11 `shouldBe` 144
  it "漸化式の表 (0〜12ヶ月目)" $
    map fib [0 .. 12] `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233]
  it "lateRabbits 12 == 60 (最初の子が 3ヶ月目)" $ lateRabbits 12 `shouldBe` 60
