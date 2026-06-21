-- | fp8.md 「Num — 数値リテラルの正体」節の instance Num Nat.
module Fp8.NumNatSpec (spec) where

import Test.Hspec

data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Num Nat where
  -- 数値リテラルの変換: 0 を Zero に, n を Succ の n 段重ねにする
  fromInteger n
    | n <= 0    = Zero
    | otherwise = Succ (fromInteger (n - 1))
  -- 足し算・掛け算
  Zero   + m = m
  Succ n + m = Succ (n + m)
  Zero   * _ = Zero
  Succ n * m = m + n * m
  -- 絶対値・符号 (ℕ では自明)
  abs    n    = n
  signum Zero = Zero
  signum _    = Succ Zero
  -- 加法逆元 (負の数) は ℕ に存在しない
  negate _ = error "Nat: 負の数は表せない"

spec :: Spec
spec = describe "Fp8.NumNat (Nat の Num インスタンス)" $ do
  it "3 :: Nat == Succ (Succ (Succ Zero)) (数値リテラルは fromInteger で展開)" $
    (3 :: Nat) `shouldBe` Succ (Succ (Succ Zero))
  it "fromInteger 0 == Zero" $
    (0 :: Nat) `shouldBe` Zero
  it "2 + 1 == (3 :: Nat)" $
    (2 + 1 :: Nat) `shouldBe` 3
  it "2 * 3 == (6 :: Nat)" $
    (2 * 3 :: Nat) `shouldBe` 6
  it "Zero は加法の単位元: 0 + 5 == (5 :: Nat)" $
    (0 + 5 :: Nat) `shouldBe` 5
