-- | fp5.md 「Exercise CH5-6」パーセプトロンによる論理ゲート.
module Fp5.PerceptronSpec (spec) where

import Prelude hiding (or)
import Test.Hspec

-- 1. パターンマッチによる or
or :: Bool -> Bool -> Bool
or False False = False
or _     _     = True

-- 2. 一般化したパーセプトロン (重み w1,w2 とバイアス b を引数に取る)
perceptron :: Double -> Double -> Double -> Double -> Double -> Bool
perceptron w1 w2 b x1 x2 = w1 * x1 + w2 * x2 + b >= 0

-- 3. 部分適用で重み・バイアスを固定するだけ
andGate, orGate, nandGate :: Double -> Double -> Bool
andGate  = perceptron 0.5    0.5    (-0.7)
orGate   = perceptron 0.5    0.5    (-0.2)
nandGate = perceptron (-0.5) (-0.5) 0.7

-- 4. Bool を Double に変換してゲートを合成する
toD :: Bool -> Double
toD True  = 1
toD False = 0

xorGate :: Double -> Double -> Bool
xorGate x1 x2 = andGate (toD (nandGate x1 x2)) (toD (orGate x1 x2))

spec :: Spec
spec = describe "Fp5.Perceptron (Exercise CH5-6)" $ do
  describe "or (パターンマッチ)" $ do
    it "False False == False" $ or False False `shouldBe` False
    it "True  False == True"  $ or True  False `shouldBe` True
    it "False True  == True"  $ or False True  `shouldBe` True
    it "True  True  == True"  $ or True  True  `shouldBe` True
  describe "andGate (部分適用)" $ do
    it "0 0 == False" $ andGate 0 0 `shouldBe` False
    it "1 0 == False" $ andGate 1 0 `shouldBe` False
    it "0 1 == False" $ andGate 0 1 `shouldBe` False
    it "1 1 == True"  $ andGate 1 1 `shouldBe` True
  describe "orGate (部分適用)" $ do
    it "0 0 == False" $ orGate 0 0 `shouldBe` False
    it "1 0 == True"  $ orGate 1 0 `shouldBe` True
    it "0 1 == True"  $ orGate 0 1 `shouldBe` True
    it "1 1 == True"  $ orGate 1 1 `shouldBe` True
  describe "nandGate (部分適用)" $ do
    it "0 0 == True"  $ nandGate 0 0 `shouldBe` True
    it "1 0 == True"  $ nandGate 1 0 `shouldBe` True
    it "0 1 == True"  $ nandGate 0 1 `shouldBe` True
    it "1 1 == False" $ nandGate 1 1 `shouldBe` False
  describe "xorGate (ゲートの合成)" $ do
    it "0 0 == False" $ xorGate 0 0 `shouldBe` False
    it "1 0 == True"  $ xorGate 1 0 `shouldBe` True
    it "0 1 == True"  $ xorGate 0 1 `shouldBe` True
    it "1 1 == False" $ xorGate 1 1 `shouldBe` False
