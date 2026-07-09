-- | fp10.md Exercise CH10-1 「<*> で 2 つの Maybe を掛け合わせる」.
module Fp10.Ex101Spec (spec) where

import Test.Hspec

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe x y = (*) <$> x <*> y

spec :: Spec
spec = describe "Fp10.Exercise CH10-1" $ do
  it "mulMaybe (Just 6) (Just 7) == Just 42" $
    mulMaybe (Just 6) (Just 7) `shouldBe` Just 42
  it "mulMaybe (Just 6) Nothing == Nothing" $
    mulMaybe (Just 6) Nothing `shouldBe` Nothing
  it "mulMaybe Nothing (Just 7) == Nothing" $
    mulMaybe Nothing (Just 7) `shouldBe` Nothing
