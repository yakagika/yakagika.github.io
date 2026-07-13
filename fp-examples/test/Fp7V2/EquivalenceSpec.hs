-- | fp7v2.md 「同値関係 — 「同じ」の一般化」節.
--   congruent7 は同値関係, nearBy は推移律が破れる.
module Fp7V2.EquivalenceSpec (spec) where

import Test.Hspec

-- ∀x ∈ xs. p x の有限版 (内包表記の節より)
forallOn :: [a] -> (a -> Bool) -> Bool
forallOn xs p = and [ p x | x <- xs ]

-- 7 で割った余りが等しい (「同じ曜日」)
congruent7 :: Int -> Int -> Bool
congruent7 x y = x `mod` 7 == y `mod` 7

-- 差が 1 以下 (「近い」)
nearBy :: Int -> Int -> Bool
nearBy x y = abs (x - y) <= 1

spec :: Spec
spec = describe "Fp7V2.Equivalence" $ do
  describe "congruent7 は同値関係 (0..30 の全数検査)" $ do
    it "反射律" $
      forallOn [0..30] (\x -> congruent7 x x) `shouldBe` True
    it "対称律" $
      and [ congruent7 y x | x <- [0..30], y <- [0..30]
          , congruent7 x y ] `shouldBe` True
    it "推移律" $
      and [ congruent7 x z | x <- [0..30], y <- [0..30], z <- [0..30]
          , congruent7 x y, congruent7 y z ] `shouldBe` True

  describe "nearBy は同値関係ではない" $ do
    it "反射律は満たす" $
      forallOn [0..30] (\x -> nearBy x x) `shouldBe` True
    it "1 R 2 かつ 2 R 3" $
      (nearBy 1 2 && nearBy 2 3) `shouldBe` True
    it "しかし 1 R 3 ではない (推移律の反例)" $
      nearBy 1 3 `shouldBe` False
