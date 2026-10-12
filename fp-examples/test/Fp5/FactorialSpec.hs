module Fp5.FactorialSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec

factorial :: Int -> Int
factorial n | n == 0    = 1
            | n > 0     = n * factorial (n - 1)
            | otherwise = error "factorial: negative input"

spec :: Spec
spec = describe "Fp5.Factorial (ガードと数式の対応)" $ do
  it "factorial 0 == 1" $ factorial 0 `shouldBe` 1
  it "factorial 5 == 120" $ factorial 5 `shouldBe` 120
  it "負の入力は error" $ evaluate (factorial (-1)) `shouldThrow` anyErrorCall
