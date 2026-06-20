-- | fp8.md 「マグマ」節のコード例.
module Fp8.MagmaSpec (spec) where

import Test.Hspec

class Magma a where
  (|*|) :: a -> a -> a   -- 数式の · に対応する閉じた二項演算

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

instance Magma RPS where
  Rock     |*| Scissors = Rock
  Scissors |*| Rock     = Rock
  Paper    |*| Rock     = Paper
  Rock     |*| Paper    = Paper
  Scissors |*| Paper    = Scissors
  Paper    |*| Scissors = Scissors
  a        |*| _        = a

spec :: Spec
spec = describe "Fp8.Magma" $ do
  describe "じゃんけん (|*|)" $ do
    it "Rock |*| Scissors == Rock"      $ (Rock |*| Scissors) `shouldBe` Rock
    it "Paper |*| Rock == Paper"        $ (Paper |*| Rock) `shouldBe` Paper
    it "Scissors |*| Paper == Scissors" $ (Scissors |*| Paper) `shouldBe` Scissors
    it "あいこは左 (Rock |*| Rock == Rock)" $ (Rock |*| Rock) `shouldBe` Rock
  describe "非結合律 (マグマだが半群ではない)" $ do
    it "(Rock |*| Paper) |*| Scissors == Scissors" $
      ((Rock |*| Paper) |*| Scissors) `shouldBe` Scissors
    it "Rock |*| (Paper |*| Scissors) == Rock" $
      (Rock |*| (Paper |*| Scissors)) `shouldBe` Rock
    it "結合律は成り立たない" $
      (((Rock |*| Paper) |*| Scissors) /= (Rock |*| (Paper |*| Scissors))) `shouldBe` True
