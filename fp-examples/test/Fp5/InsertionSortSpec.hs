module Fp5.InsertionSortSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

-- fp5 §再帰 の worked example (明示再帰による挿入ソート)
insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

spec :: Spec
spec = describe "Fp5.InsertionSort" $ do
  it "insertionSort [3,1,2] == [1,2,3]" $ insertionSort [3,1,2] `shouldBe` [1,2,3]
  it "空リストはそのまま"               $ insertionSort []      `shouldBe` []
  it "整列済みはそのまま"               $ insertionSort [1,2,3,4] `shouldBe` [1,2,3,4]
  it "逆順も昇順になる"                 $ insertionSort [5,4,3,2,1] `shouldBe` [1,2,3,4,5]
  it "重複があっても安定に並ぶ"         $ insertionSort [3,1,3,2,1] `shouldBe` [1,1,2,3,3]
  it "Data.List.sort と一致 (QuickCheck)" $ property $ \xs ->
    insertionSort xs == sort (xs :: [Int])
