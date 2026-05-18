module Fp5.TypeSpec (spec) where

import Test.Hspec

type Bottom = Double
type Height = Double

data Rectangle = Rectangle {bottom :: Bottom
                           ,height :: Height}
    deriving (Show, Eq)

mkRectangle :: Bottom -> Height -> Rectangle
mkRectangle b h = Rectangle {bottom = b, height = h}


spec :: Spec
spec = describe "Fp5.TypeSpeck" $ do
  describe "mkRectangle" $ do
    it "mkRectangle 3 4 == Rectangle 3 4" $
      mkRectangle 3 4 `shouldBe` Rectangle 3 4

