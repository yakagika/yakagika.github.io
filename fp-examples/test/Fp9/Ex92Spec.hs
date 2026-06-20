-- | fp9.md Exercise CH9-2 「理由つきの検証 checkAge」.
module Fp9.Ex92Spec (spec) where

import Test.Hspec

checkAge :: Int -> Either String Int
checkAge n
  | n < 0     = Left "年齢が負です"
  | n > 150   = Left "年齢が大きすぎます"
  | otherwise = Right n

spec :: Spec
spec = describe "Fp9.Exercise CH9-2" $ do
  it "checkAge 30 == Right 30" $
    checkAge 30 `shouldBe` Right 30
  it "checkAge (-1) == Left 年齢が負です" $
    checkAge (-1) `shouldBe` Left "年齢が負です"
  it "checkAge 200 == Left 年齢が大きすぎます" $
    checkAge 200 `shouldBe` Left "年齢が大きすぎます"
