-- | fp9.md 「最初の関手 — 一点圏から一点圏へ」節のコード例.
--   リストの一点圏から (Int, +, 0) の一点圏への関手 length (= モノイド準同型)
--   が合成 (++ → +) と恒等射 ([] → 0) を保つことの機械検査.
module Fp9.LengthFunctorSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp9.LengthFunctor (一点圏どうしの関手 length)" $ do
  it "射の対応: length [10,20,30] == 3" $
    length [10, 20, 30 :: Int] `shouldBe` 3
  it "合成を保つ: length (xs ++ ys) == length xs + length ys" $
    length ([1, 2] ++ [3, 4, 5 :: Int])
      `shouldBe` length [1, 2 :: Int] + length [3, 4, 5 :: Int]
  it "恒等射を保つ: length [] == 0" $
    length ([] :: [Int]) `shouldBe` 0
