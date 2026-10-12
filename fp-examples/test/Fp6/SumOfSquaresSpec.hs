module Fp6.SumOfSquaresSpec (spec) where

import Test.Hspec

square :: Int -> Int
square x = x * x

sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map square

-- 2 乗してから 1 を足す. 左辺の形 (リストを 2 回走査する)
squarePlusOneTwice :: [Int] -> [Int]
squarePlusOneTwice = map (+ 1) . map square

-- 右辺の形 (リストを 1 回走査する)
squarePlusOneOnce :: [Int] -> [Int]
squarePlusOneOnce = map ((+ 1) . square)

spec :: Spec
spec = describe "Fp6.SumOfSquares (合成と等式推論)" $ do
  it "sumOfSquares [1,2,3] == 14" $ sumOfSquares [1, 2, 3] `shouldBe` 14
  it "squarePlusOneTwice [1,2,3] == [2,5,10]" $
    squarePlusOneTwice [1, 2, 3] `shouldBe` [2, 5, 10]
  it "squarePlusOneOnce [1,2,3] == [2,5,10]" $
    squarePlusOneOnce [1, 2, 3] `shouldBe` [2, 5, 10]
  it "map の合成則: 左辺と右辺が一致" $
    all (\xs -> squarePlusOneTwice xs == squarePlusOneOnce xs)
        [[], [0], [1, 2, 3], [-4, 7, 0, 2]] `shouldBe` True
