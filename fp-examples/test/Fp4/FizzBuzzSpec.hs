-- | lectures/fp/fp4.md 「再帰」節の練習問題2 (FizzBuzz).
module Fp4.FizzBuzzSpec (spec) where

import Test.Hspec

fizzBuzz :: [Int] -> [String]
fizzBuzz []     = []
fizzBuzz (x:xs) = fb x : fizzBuzz xs
  where
    fb n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod`  3 == 0 = "Fizz"
      | n `mod`  5 == 0 = "Buzz"
      | otherwise       = show n

spec :: Spec
spec = describe "Fp4.FizzBuzz" $ do
  it "fizzBuzz [] == []" $
    fizzBuzz [] `shouldBe` []
  it "fizzBuzz [1..15] (fp4.md の期待値)" $
    fizzBuzz [1 .. 15] `shouldBe`
      [ "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8"
      , "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz"
      ]
  it "15 の倍数では FizzBuzz" $
    fizzBuzz [15, 30, 45] `shouldBe` ["FizzBuzz", "FizzBuzz", "FizzBuzz"]
