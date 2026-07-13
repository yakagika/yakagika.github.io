-- | fp7.md 「有限集合での ∀・∃ の機械検査」節の forallOn / existsOn.
module Fp7.ForallExistsSpec (spec) where

import Test.Hspec

-- ∀x ∈ xs. p x の有限版
forallOn :: [a] -> (a -> Bool) -> Bool
forallOn xs p = and [ p x | x <- xs ]

-- ∃x ∈ xs. p x の有限版
existsOn :: [a] -> (a -> Bool) -> Bool
existsOn xs p = or [ p x | x <- xs ]

spec :: Spec
spec = describe "Fp7.ForallExists" $ do
  describe "forallOn (∀ の有限版)" $ do
    it "すべて偶数なら True" $
      forallOn [2, 4, 6, 8, 10 :: Int] even `shouldBe` True
    it "奇数が混ざれば False" $
      forallOn [1 .. 10 :: Int] even `shouldBe` False

  describe "existsOn (∃ の有限版)" $ do
    it "x*x == 25 となる x が 1..10 に存在" $
      existsOn [1 .. 10 :: Int] (\x -> x * x == 25) `shouldBe` True
    it "x*x == 26 となる x は存在しない" $
      existsOn [1 .. 10 :: Int] (\x -> x * x == 26) `shouldBe` False

  describe "all / any との一致" $ do
    it "forallOn xs p == all p xs" $
      forallOn [1 .. 10 :: Int] even `shouldBe` all even [1 .. 10 :: Int]
    it "existsOn xs p == any p xs" $
      existsOn [1 .. 10 :: Int] even `shouldBe` any even [1 .. 10 :: Int]
