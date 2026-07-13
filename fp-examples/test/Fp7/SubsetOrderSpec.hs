-- | fp7.md Exercise CH7-7 (部分集合関係は半順序 — しかし全順序ではない) の回答例.
module Fp7.SubsetOrderSpec (spec) where

import Test.Hspec

subsetOf :: [Int] -> [Int] -> Bool
subsetOf xs ys = and [ x `elem` ys | x <- xs ]

-- {1,2,3} の部分集合 8 個
subsets :: [[Int]]
subsets = [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]

sameSet :: [Int] -> [Int] -> Bool
sameSet xs ys = subsetOf xs ys && subsetOf ys xs

spec :: Spec
spec = describe "Fp7.SubsetOrder (Exercise CH7-7)" $ do
  describe "subsetOf" $ do
    it "[1,2] ⊆ [1,2,3]"        $ ([1,2] `subsetOf` [1,2,3]) `shouldBe` True
    it "[1,4] ⊄ [1,2,3]"        $ ([1,4] `subsetOf` [1,2,3]) `shouldBe` False
    it "空集合は任意の集合の部分集合" $ ([] `subsetOf` [1,2,3]) `shouldBe` True

  describe "subsetOf は subsets 上で半順序" $ do
    it "反射律" $
      and [ s `subsetOf` s | s <- subsets ] `shouldBe` True
    it "推移律" $
      and [ a `subsetOf` c | a <- subsets, b <- subsets, c <- subsets
          , a `subsetOf` b, b `subsetOf` c ] `shouldBe` True
    it "反対称律 (sameSet の意味で)" $
      and [ sameSet a b | a <- subsets, b <- subsets
          , a `subsetOf` b, b `subsetOf` a ] `shouldBe` True

  describe "subsetOf は全順序ではない" $ do
    it "[1] と [2] は比較不能" $
      ([1] `subsetOf` [2] || [2] `subsetOf` [1]) `shouldBe` False
