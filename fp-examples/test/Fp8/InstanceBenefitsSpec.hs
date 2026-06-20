-- | fp8.md 「代数のインスタンスにする利点」節のコード例 (foldMap / fold / stimes).
module Fp8.InstanceBenefitsSpec (spec) where

import Test.Hspec
import Data.Foldable (fold)
import Data.Semigroup (stimes)

newtype Add = Add Int deriving (Show, Eq)
instance Semigroup Add where Add a <> Add b = Add (a + b)
instance Monoid    Add where mempty = Add 0

spec :: Spec
spec = describe "Fp8.InstanceBenefits (instance にする利点)" $ do
  describe "foldMap / fold (Foldable は Monoid を要求)" $ do
    it "foldMap Add [1,2,3] == Add 6" $
      foldMap Add [1, 2, 3] `shouldBe` Add 6
    it "fold [Add 1, Add 2, Add 3] == Add 6" $
      fold [Add 1, Add 2, Add 3] `shouldBe` Add 6
    it "リスト以外の Foldable でも動く: foldMap Add (Just 5) == Add 5" $
      foldMap Add (Just 5) `shouldBe` Add 5
    it "空コンテナは mempty: foldMap Add [] == Add 0" $
      foldMap Add ([] :: [Int]) `shouldBe` Add 0
  describe "stimes (Semigroup, n 回繰り返し)" $ do
    it "stimes 3 (Add 2) == Add 6" $
      stimes (3 :: Int) (Add 2) `shouldBe` Add 6
    it "stimes 3 [1] == [1,1,1]" $
      stimes (3 :: Int) [1 :: Int] `shouldBe` [1, 1, 1]
