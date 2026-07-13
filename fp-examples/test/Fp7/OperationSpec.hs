-- | fp7.md 「演算 — 台の上で閉じた関数」節のコード例.
module Fp7.OperationSpec (spec) where

import Test.Hspec

-- 2 項演算: 台 Int の中で閉じている
plus :: Int -> Int -> Int
plus x y = x + y

-- 1 項演算
neg :: Int -> Int
neg x = negate x

-- 0 項演算 = 定数
zero :: Int
zero = 0

-- 演算ではない関数: 台 [Int] から外 (Int) へ出ていく
len :: [Int] -> Int
len xs = length xs

spec :: Spec
spec = describe "Fp7.Operation (演算 = 台の上で閉じた関数)" $ do
  it "2 項演算 plus" $ plus 3 4 `shouldBe` 7
  it "1 項演算 neg"  $ neg 5 `shouldBe` (-5)
  it "0 項演算 (定数) zero" $ zero `shouldBe` 0
  it "len は関数だが [Int] の上の演算ではない (行き先が Int)" $
    len [1, 2, 3] `shouldBe` 3
