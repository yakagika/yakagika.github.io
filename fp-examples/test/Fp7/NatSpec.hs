-- | fp7.md 「再帰的データ型」節の Nat (ペアノ自然数).
module Fp7.NatSpec (spec) where

import Test.Hspec

data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Nat を Int に変換する (構造をたどって 1 ずつ数える)
toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

-- 自然数の足し算 (m の上に Succ を n 個積み直す)
add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

spec :: Spec
spec = describe "Fp7.Nat" $ do
  it "toInt (Succ (Succ (Succ Zero))) == 3" $
    toInt (Succ (Succ (Succ Zero))) `shouldBe` 3
  it "toInt (add three two) == 5" $
    toInt (add (Succ (Succ (Succ Zero))) (Succ (Succ Zero))) `shouldBe` 5
  it "add Zero m == m (左単位元)" $
    add Zero (Succ Zero) `shouldBe` Succ Zero
  it "add (Succ Zero) (Succ Zero) == Succ (Succ Zero)" $
    add (Succ Zero) (Succ Zero) `shouldBe` Succ (Succ Zero)
