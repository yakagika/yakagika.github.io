{-# LANGUAGE ScopedTypeVariables #-}
-- | fp5.md 「式と文」節のコード例を検証する.
--
-- 対応する教材のコードブロック:
--
--   * 「全てが式」の absPlus1 (if を式として埋め込む例)
--
-- ghci セッション例や故意の文法エラー例 (Python の if 文) は検証対象外.
module Fp5.ExpressionsSpec (spec) where

import Test.Hspec

-- 教材「全てが式」のコードブロックと同じ定義
absPlus1 :: Int -> Int
absPlus1 x = (if x >= 0 then x else -x) + 1

spec :: Spec
spec = describe "fp5 式と文" $ do
  it "absPlus1 3 = 4 (if を式として利用)" $
    absPlus1 3 `shouldBe` 4
  it "absPlus1 (-5) = 6" $
    absPlus1 (-5) `shouldBe` 6
