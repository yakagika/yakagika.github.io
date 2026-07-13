-- | fp7v2.md 「関係 — 直積の部分集合」節の divides / dividesPairs.
module Fp7V2.RelationDividesSpec (spec) where

import Test.Hspec

-- 判定関数: a は b を割り切るか
divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

-- 関係 D の {1..n} の範囲での全体像 (内包表記そのまま)
dividesPairs :: Int -> [(Int, Int)]
dividesPairs n = [ (a, b) | a <- [1..n], b <- [1..n], a `divides` b ]

spec :: Spec
spec = describe "Fp7V2.RelationDivides" $ do
  describe "divides (判定関数)" $ do
    it "3 `divides` 12 == True"  $ (3 `divides` 12) `shouldBe` True
    it "5 `divides` 12 == False" $ (5 `divides` 12) `shouldBe` False

  describe "dividesPairs (関係の有限版)" $ do
    it "dividesPairs 4 (fp7v2.md の期待値)" $
      dividesPairs 4 `shouldBe`
        [(1,1), (1,2), (1,3), (1,4), (2,2), (2,4), (3,3), (4,4)]
    it "判定関数との対応: (a,b) ∈ D ⟺ divides a b" $
      dividesPairs 6 `shouldBe`
        [ (a, b) | a <- [1..6], b <- [1..6], a `divides` b ]
