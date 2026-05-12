-- | fp3.md 「論理型 (Bool)」節の練習問題.
-- 過去にここの回答例で `False` と書かれていたところが実は `True` だったバグがあり,
-- このテストが回帰防止の盾になる.
module Fp3.BooleanPracticeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp3.BooleanPractice" $ do
  let x = 101 :: Int
      y = 202 :: Int
      p = x `mod` 2 == 0   -- x が偶数か
      q = y `mod` 2 == 0   -- y が偶数か
  it "P(x) = xが偶数 → False (x=101)" $
    p `shouldBe` False
  it "Q(y) = yが偶数 → True (y=202)" $
    q `shouldBe` True
  it "P(x) ∧ Q(y) → False" $
    (p && q) `shouldBe` False
  it "P(x) ∨ Q(y) → True" $
    (p || q) `shouldBe` True
  it "P(x) ⇒ ¬Q(y) (x偶数 ⇒ y奇数) → True (前件偽より空虚な真)" $
    (not p || not q) `shouldBe` True
  it "(x + y) mod 2 /= 0 (x+y が奇数) → True" $
    ((x + y) `mod` 2 /= 0) `shouldBe` True
