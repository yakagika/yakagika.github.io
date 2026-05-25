-- | fp5.md 「練習問題 (関数総合)」のパーセプトロン (OR 回路).
module Fp5.PerceptronSpec (spec) where

import Test.Hspec

perceptronOR :: Bool -> Bool -> Bool
perceptronOR x1 x2
  | threshold >= 0 = True
  | otherwise      = False
  where
    g True  = 1 :: Double
    g False = 0
    threshold = 0.5 * g x1 + 0.5 * g x2 - 0.2

spec :: Spec
spec = describe "Fp5.Perceptron" $ do
  it "False OR False == False" $
    perceptronOR False False `shouldBe` False
  it "True OR False == True" $
    perceptronOR True False `shouldBe` True
  it "False OR True == True" $
    perceptronOR False True `shouldBe` True
  it "True OR True == True" $
    perceptronOR True True `shouldBe` True
