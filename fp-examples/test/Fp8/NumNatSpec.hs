-- | fp8.md 「Num — 数値リテラルの正体」節の instance Num Nat.
module Fp8.NumNatSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Num Nat where
  -- 数値リテラルの変換: 0 を Zero に, n を Succ の n 段重ねにする
  fromInteger n
    | n == 0    = Zero
    | n > 0     = Succ (fromInteger (n - 1))
    | otherwise = error "Nat: 負の整数は表せない"
  -- 足し算・掛け算
  Zero   + m = m
  Succ n + m = Succ (n + m)
  Zero   * _ = Zero
  Succ n * m = m + n * m
  -- 絶対値 abs はそのまま (ℕ は非負なので符号を外す操作は不要)
  abs    n    = n
  -- signum は符号を返すメソッド (正→1, 0→0, 負→-1). ℕ には負がないので
  -- Zero なら 0, それ以外は 1 (= Succ Zero) のみ
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
  it "負の整数は表せない: fromInteger (-1) はエラー" $
    evaluate (fromInteger (-1) :: Nat) `shouldThrow` anyErrorCall
