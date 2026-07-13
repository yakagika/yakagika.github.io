-- | fp7.md 「順序関係 — 「並べる」の一般化」節.
--   divides は半順序だが全順序ではない.
module Fp7.OrderDividesSpec (spec) where

import Test.Hspec

divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

spec :: Spec
spec = describe "Fp7.OrderDivides" $ do
  describe "divides は {1..20} 上で半順序" $ do
    it "反射律" $
      and [ a `divides` a | a <- [1..20] ] `shouldBe` True
    it "反対称律" $
      and [ a == b | a <- [1..20], b <- [1..20]
          , a `divides` b, b `divides` a ] `shouldBe` True
    it "推移律" $
      and [ a `divides` c | a <- [1..20], b <- [1..20], c <- [1..20]
          , a `divides` b, b `divides` c ] `shouldBe` True

  describe "divides は全順序ではない" $ do
    it "2 と 3 は比較不能" $
      (2 `divides` 3 || 3 `divides` 2) `shouldBe` False
