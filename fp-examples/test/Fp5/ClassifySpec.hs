{-# LANGUAGE ScopedTypeVariables #-}
-- | fp5.md 「分岐」節末尾の note「4つの分岐記法の使い分け」のコード例を検証する.
--
-- 対応する教材のコードブロック:
--
--   * パターンマッチと case式の使い分けを示す classify
--     (let で束縛した値 m に case式で分岐する例)
module Fp5.ClassifySpec (spec) where

import Test.Hspec

-- 教材「パターンマッチと case式の使い分け」のコードブロックと同じ定義
classify :: Int -> String
classify n =
    let m = n * 2
    in case m of
        0 -> "zero"
        2 -> "two"
        _ -> "other"

spec :: Spec
spec = describe "Fp5.Classify" $ do
  it "classify 0 == \"zero\"" $
    classify 0 `shouldBe` "zero"
  it "classify 1 == \"two\" (1 * 2 = 2)" $
    classify 1 `shouldBe` "two"
  it "classify 5 == \"other\"" $
    classify 5 `shouldBe` "other"
