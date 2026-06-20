-- | fp9.md Exercise CH9-3 「Maybe を Either へ変換 (toEither)」.
module Fp9.Ex93Spec (spec) where

import Test.Hspec

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

toEither :: String -> Maybe a -> Either String a
toEither reason Nothing  = Left reason
toEither _      (Just x) = Right x

spec :: Spec
spec = describe "Fp9.Exercise CH9-3" $ do
  it "toEither reason (safeDiv 10 2) == Right 5" $
    toEither "0 では割れません" (safeDiv 10 2) `shouldBe` Right 5
  it "toEither reason (safeDiv 10 0) == Left reason" $
    toEither "0 では割れません" (safeDiv 10 0) `shouldBe` Left "0 では割れません"
