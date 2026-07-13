-- | fp9.md Exercise CH9-3 「理由つきの検証 checkAge (専用エラー型)」.
module Fp9.Ex93Spec (spec) where

import Test.Hspec

-- ① 起こりうる失敗を直和型で列挙する
data AgeError = Negative | TooLarge
  deriving (Show, Eq)

-- ② その型を Left に載せて検証する
checkAge :: Int -> Either AgeError Int
checkAge n
  | n < 0     = Left Negative
  | n > 150   = Left TooLarge
  | otherwise = Right n

-- ③ 表示は値と分離し, render 関数で与える
renderAgeError :: AgeError -> String
renderAgeError Negative = "年齢が負です"
renderAgeError TooLarge = "年齢が大きすぎます"

spec :: Spec
spec = describe "Fp9.Exercise CH9-3" $ do
  it "checkAge 30 == Right 30" $
    checkAge 30 `shouldBe` Right 30
  it "checkAge (-1) == Left Negative" $
    checkAge (-1) `shouldBe` Left Negative
  it "checkAge 200 == Left TooLarge" $
    checkAge 200 `shouldBe` Left TooLarge
  it "renderAgeError Negative == 年齢が負です" $
    renderAgeError Negative `shouldBe` "年齢が負です"
  it "either renderAgeError show (checkAge (-1)) == 年齢が負です" $
    either renderAgeError show (checkAge (-1)) `shouldBe` "年齢が負です"
