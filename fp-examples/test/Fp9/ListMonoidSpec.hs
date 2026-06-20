-- | fp9.md 「リスト」節のコード例.
module Fp9.ListMonoidSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp9.ListMonoid" $ do
  it "([1,2] <> [3,4]) :: [Int] == [1,2,3,4]" $
    (([1,2] <> [3,4]) :: [Int]) `shouldBe` [1,2,3,4]
  it "([] <> [1,2,3]) :: [Int] == [1,2,3]" $
    (([] <> [1,2,3]) :: [Int]) `shouldBe` [1,2,3]
  it "([1,2,3] <> []) :: [Int] == [1,2,3]" $
    (([1,2,3] <> []) :: [Int]) `shouldBe` [1,2,3]
  it "mconcat [[1],[2],[3]] :: [Int] == [1,2,3]" $
    (mconcat [[1],[2],[3]] :: [Int]) `shouldBe` [1,2,3]
  it "(mempty :: [Int]) == []" $
    (mempty :: [Int]) `shouldBe` []
