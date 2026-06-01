{-# LANGUAGE ScopedTypeVariables #-}
-- | fp5.md 「練習問題(関数総合)」Exercise CH5-5 のコード例を検証する.
--
-- 対応する教材のコードブロック:
--
--   * grade  : 点数 → 評価ランク (ガード)
--   * grades : リストの各要素をランクへ変換 (再帰, map 不使用)
module Fp5.GradeSpec (spec) where

import Test.Hspec

-- 教材 Exercise CH5-5 回答例と同じ定義
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

grades :: [Int] -> [String]
grades []     = []
grades (x:xs) = grade x : grades xs

spec :: Spec
spec = describe "Fp5.Grade" $ do
  it "grade 95 == \"A\"" $ grade 95 `shouldBe` "A"
  it "grade 72 == \"C\"" $ grade 72 `shouldBe` "C"
  it "grade 40 == \"F\"" $ grade 40 `shouldBe` "F"
  it "境界値 90/80/70/60" $ do
    grade 90 `shouldBe` "A"
    grade 80 `shouldBe` "B"
    grade 70 `shouldBe` "C"
    grade 60 `shouldBe` "D"
    grade 59 `shouldBe` "F"
  it "grades [95,82,71,55] == [\"A\",\"B\",\"C\",\"F\"]" $
    grades [95, 82, 71, 55] `shouldBe` ["A", "B", "C", "F"]
  it "grades [] == []" $
    grades [] `shouldBe` []
