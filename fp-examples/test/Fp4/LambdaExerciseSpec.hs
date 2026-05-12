-- | fp4.md 「無名関数 (ラムダ式)」節の練習問題.
module Fp4.LambdaExerciseSpec (spec) where

import Test.Hspec

addThree :: [Int] -> [Int]
addThree xs = map (\x -> x + 3) xs

onlyEven :: [Int] -> [Int]
onlyEven xs = filter (\x -> x `mod` 2 == 0) xs

sumAbs :: [Int] -> Int
sumAbs xs = sum (map (\x -> abs x) xs)

spec :: Spec
spec = describe "Fp4.LambdaExercise" $ do
  it "addThree [1,2,3] == [4,5,6]" $
    addThree [1, 2, 3] `shouldBe` [4, 5, 6]
  it "onlyEven [1,2,3,4,5,6] == [2,4,6]" $
    onlyEven [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]
  it "sumAbs [-3,4,-1,2] == 10" $
    sumAbs [-3, 4, -1, 2] `shouldBe` 10
