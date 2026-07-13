-- | fp7v2.md 「どこにも循環はない — 集合を下から立ち上げる」節の生成作用素 Φ.
module Fp7V2.NatStepSpec (spec) where

import Test.Hspec

data Nat = Zero | Succ Nat deriving (Show, Eq)

-- 生成作用素 Φ: 構成子を 1 段だけ適用して作れる値の集合 (リスト表現)
step :: [Nat] -> [Nat]
step xs = Zero : [ Succ n | n <- xs ]

spec :: Spec
spec = describe "Fp7V2.NatStep (生成作用素 Φ)" $ do
  it "Φ(∅) = {Zero}" $
    step [] `shouldBe` [Zero]
  it "Φ²(∅) = {Zero, Succ Zero}" $
    step (step []) `shouldBe` [Zero, Succ Zero]
  it "Φ³(∅) = {Zero, Succ Zero, Succ (Succ Zero)}" $
    step (step (step [])) `shouldBe` [Zero, Succ Zero, Succ (Succ Zero)]
  it "Φ^10(∅) は 10 個の値を持つ" $
    length (iterate step [] !! 10) `shouldBe` 10
  it "近似列は包含で増える: Φ^n(∅) ⊆ Φ^(n+1)(∅) (n = 0..5)" $
    and [ all (`elem` (iterate step [] !! (n + 1))) (iterate step [] !! n)
        | n <- [0 .. 5] ] `shouldBe` True
