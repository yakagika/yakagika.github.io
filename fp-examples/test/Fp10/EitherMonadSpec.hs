-- | fp10.md 「Maybe / Either モナド — 失敗の連鎖」節のコード例.
--   Either は失敗理由を専用の列挙型で運ぶ (String でなく型で場合分けする).
module Fp10.EitherMonadSpec (spec) where

import Test.Hspec

-- なぜ失敗したかを型で表す (String でなく専用の列挙型)
data CalcError
  = DivByZero   -- 0 で割ろうとした
  | Negative    -- 負の入力は許さない
  deriving (Show, Eq)

safeDivE :: Int -> Int -> Either CalcError Int
safeDivE _ 0 = Left DivByZero
safeDivE x y = Right (x `div` y)

checkPos :: Int -> Either CalcError Int
checkPos n
  | n < 0     = Left Negative
  | otherwise = Right n

-- 2 つの入力を検査してから割る. どこで失敗しても, 最初のエラーが返る.
calcE :: Int -> Int -> Either CalcError Int
calcE x y = do
  x' <- checkPos x
  y' <- checkPos y
  safeDivE x' y'

spec :: Spec
spec = describe "Fp10.EitherMonad" $ do
  it "calcE 100 5 == Right 20" $
    calcE 100 5 `shouldBe` Right 20
  it "calcE 100 0 == Left DivByZero (0 除算の理由が載る)" $
    calcE 100 0 `shouldBe` Left DivByZero
  it "calcE (-1) 5 == Left Negative (最初の検査で失敗)" $
    calcE (-1) 5 `shouldBe` Left Negative
  it "Left が出た時点で後続を素通しする" $
    (checkPos (-1) >>= \x -> safeDivE x 0) `shouldBe` Left Negative
