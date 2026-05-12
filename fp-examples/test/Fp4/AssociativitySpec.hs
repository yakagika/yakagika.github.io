-- | fp4.md 「結合性」節の練習問題.
-- 同一スクリプトに 4 つの演算子を併存させ, 優先順位の違いで結果が変わることを検証する.
module Fp4.AssociativitySpec (spec) where

import Test.Hspec

-- 問題 1: infixr 5 .+ で 1 .+ 2 .+ 3
(.+) :: Int -> Int -> Int
x .+ y = x + y
infixr 5 .+

-- 問題 2: セット A (通常の + * と同じ優先順位関係)
(.+.) :: Int -> Int -> Int
x .+. y = x + y
infixl 6 .+.

(.*.) :: Int -> Int -> Int
x .*. y = x * y + 1
infixl 7 .*.

-- 問題 2: セット B (逆の優先順位)
(+:) :: Int -> Int -> Int
x +: y = x + y
infixl 7 +:

(*:) :: Int -> Int -> Int
x *: y = x * y + 1
infixl 6 *:

spec :: Spec
spec = describe "Fp4.Associativity" $ do
  it "1 .+ 2 .+ 3 == 6 (infixr で右結合)" $
    1 .+ 2 .+ 3 `shouldBe` 6

  it "セット A: 2 .+. 3 .*. 4 == 15 (.* 優先, 2 + (3*4+1))" $
    2 .+. 3 .*. 4 `shouldBe` 15

  it "セット B: 2 +: 3 *: 4 == 21 (+: 優先, (2+3)*4+1)" $
    2 +: 3 *: 4 `shouldBe` 21
