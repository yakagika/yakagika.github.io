-- | fp10.md Exercise CH10-3 「専用エラー型での検証チェーン (口座からの連続引き出し)」.
module Fp10.Ex103Spec (spec) where

import Test.Hspec

data BankError = NotEnough | Negative deriving (Show, Eq)

withdraw :: Int -> Int -> Either BankError Int
withdraw balance amount
  | amount < 0       = Left Negative
  | amount > balance = Left NotEnough
  | otherwise        = Right (balance - amount)

twice :: Int -> Int -> Int -> Either BankError Int
twice balance a b = do
  b1 <- withdraw balance a
  withdraw b1 b

spec :: Spec
spec = describe "Fp10.Exercise CH10-3" $ do
  it "twice 100 30 50 == Right 20" $
    twice 100 30 50 `shouldBe` Right 20
  it "twice 100 30 80 == Left NotEnough (2 回目で残高不足)" $
    twice 100 30 80 `shouldBe` Left NotEnough
  it "twice 100 (-1) 0 == Left Negative (1 回目で負の額)" $
    twice 100 (-1) 0 `shouldBe` Left Negative
