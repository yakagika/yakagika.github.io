-- | fp7.md 「newtype」節の `Bottom` / `Height` 例.
module Fp7.NewtypeSpec (spec) where

import Test.Hspec

newtype Bottom = Bottom Double
  deriving (Show, Eq)

newtype Height = Height Double
  deriving (Show, Eq)

data Rectangle = Rectangle Bottom Height
  deriving (Show, Eq)

newtype Positive = Positive Int
  deriving (Show, Eq)

mkRectangle :: Bottom -> Height -> Rectangle
mkRectangle b h = Rectangle b h

area :: Rectangle -> Double
area (Rectangle (Bottom b) (Height h)) = b * h

badPositive :: Positive
badPositive = Positive (-1)

spec :: Spec
spec = describe "Fp7.Newtype" $ do
  describe "mkRectangle" $
    it "Bottom と Height を区別して Rectangle を作る" $
      mkRectangle (Bottom 3.0) (Height 4.0)
        `shouldBe` Rectangle (Bottom 3.0) (Height 4.0)

  describe "area" $
    it "newtype の中身を取り出して面積を計算する" $
      area (Rectangle (Bottom 3.0) (Height 4.0)) `shouldBe` 12.0

  describe "Positive" $
    it "newtype だけでは値の妥当性は検査されない" $
      badPositive `shouldBe` Positive (-1)
