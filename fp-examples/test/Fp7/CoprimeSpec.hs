-- | fp7.md Exercise CH7-5 (互いに素 — 関係を内包表記で計算する) の回答例.
module Fp7.CoprimeSpec (spec) where

import Test.Hspec

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

coprimePairs :: Int -> [(Int, Int)]
coprimePairs n = [ (a, b) | a <- [1..n], b <- [1..n], coprime a b ]

spec :: Spec
spec = describe "Fp7.Coprime (Exercise CH7-5)" $ do
  describe "coprime (判定関数)" $ do
    it "coprime 8 15 == True"  $ coprime 8 15 `shouldBe` True
    it "coprime 6 9 == False"  $ coprime 6 9 `shouldBe` False

  describe "coprimePairs (関係の有限版)" $ do
    it "coprimePairs 4 (fp7.md の期待値)" $
      coprimePairs 4 `shouldBe`
        [ (1,1), (1,2), (1,3), (1,4)
        , (2,1), (2,3)
        , (3,1), (3,2), (3,4)
        , (4,1), (4,3)
        ]

  describe "法則の全数検査" $ do
    it "対称律 (1..20)" $
      and [ coprime b a | a <- [1..20], b <- [1..20], coprime a b ]
        `shouldBe` True
    it "反射的ではない: coprime 1 1 は True だが coprime 2 2 は False" $ do
      coprime 1 1 `shouldBe` True
      coprime 2 2 `shouldBe` False
