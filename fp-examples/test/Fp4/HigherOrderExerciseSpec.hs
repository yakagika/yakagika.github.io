-- | fp4.md 「高階関数」節の練習問題.
module Fp4.HigherOrderExerciseSpec (spec) where

import Test.Hspec

squareList :: [Int] -> [Int]
squareList xs = map (^ (2 :: Int)) xs

productList :: [Int] -> Int
productList xs = foldl (*) 1 xs

maxList :: [Int] -> [Int] -> [Int]
maxList xs ys = zipWith (\x y -> if x > y then x else y) xs ys

spec :: Spec
spec = describe "Fp4.HigherOrderExercise" $ do
  it "squareList [1,2,3,4] == [1,4,9,16]" $
    squareList [1, 2, 3, 4] `shouldBe` [1, 4, 9, 16]
  it "productList [1,2,3,4] == 24" $
    productList [1, 2, 3, 4] `shouldBe` 24
  it "maxList [1,4,3] [2,2,5] == [2,4,5]" $
    maxList [1, 4, 3] [2, 2, 5] `shouldBe` [2, 4, 5]
