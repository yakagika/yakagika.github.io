-- | fp9v2.md Exercise CH9-4 「Maybe を Either へ変換 (エラー型を選べる toEither)」.
module Fp9V2.Ex94Spec (spec) where

import Test.Hspec

data DivError = DivByZero
  deriving (Show, Eq)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- 理由の型 e を固定しない (専用エラー型でも String でも動く)
toEither :: e -> Maybe a -> Either e a
toEither reason Nothing  = Left reason
toEither _      (Just x) = Right x

spec :: Spec
spec = describe "Fp9V2.Exercise CH9-4" $ do
  it "toEither DivByZero (safeDiv 10 2) == Right 5" $
    toEither DivByZero (safeDiv 10 2) `shouldBe` Right 5
  it "toEither DivByZero (safeDiv 10 0) == Left DivByZero" $
    toEither DivByZero (safeDiv 10 0) `shouldBe` Left DivByZero
  it "toEither も String で動く (エラー型多相)" $
    toEither "0 では割れません" (safeDiv 10 2) `shouldBe` Right 5
