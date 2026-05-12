-- | fp4.md 「再帰」節の練習問題 `length2`.
module Fp4.Length2Spec (spec) where

import Test.Hspec

length2 :: [a] -> Int
length2 []     = 0
length2 (_:xs) = 1 + length2 xs

spec :: Spec
spec = describe "Fp4.Length2" $ do
  it "length2 [] == 0" $
    length2 ([] :: [Int]) `shouldBe` 0
  it "length2 [1,2,3,4,5] == 5 (fp4.md の期待値)" $
    length2 [1 :: Int, 2, 3, 4, 5] `shouldBe` 5
  it "length2 \"hello\" == 5 (fp4.md の期待値)" $
    length2 "hello" `shouldBe` 5
  it "length2 == length (任意のリスト)" $
    length2 [1 :: Int .. 100] `shouldBe` length [1 :: Int .. 100]
