-- | fp3.md 「リスト」節の練習問題.
module Fp3.ListPracticeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp3.ListPractice" $ do
  it "[1..100] の長さ == 100" $
    length [1 :: Int .. 100] `shouldBe` 100
  it "[1..100] の先頭/末尾" $ do
    head [1 :: Int .. 100] `shouldBe` 1
    last [1 :: Int .. 100] `shouldBe` 100
  it "[1,3..99] (奇数列) の長さ == 50" $
    length [1, 3 .. 99 :: Int] `shouldBe` 50
  it "[1,3..99] !! 19 == 39 (20番目の奇数)" $
    [1, 3 .. 99 :: Int] !! 19 `shouldBe` 39
  it "1 : 2 : 3 : [] == [1,2,3]" $
    1 : 2 : 3 : [] `shouldBe` ([1, 2, 3] :: [Int])
  it "length ([1,2,3] ++ [4,5,6]) == 6" $
    length ([1, 2, 3] ++ [4, 5, 6] :: [Int]) `shouldBe` 6
  it "[2,4..] !! 99 == 200 (100番目の偶数)" $
    let evens = [2, 4 ..] :: [Int]
    in  evens !! 99 `shouldBe` 200
