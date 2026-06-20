-- | fp8.md 「Eq — 等価性」節のコード例.
module Fp8.EqInstanceSpec (spec) where

import Test.Hspec

data Color = Red | Green | Blue

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

spec :: Spec
spec = describe "Fp8.EqInstance" $ do
  it "(Red == Red) == True"   $ (Red == Red)   `shouldBe` True
  it "(Red == Blue) == False" $ (Red == Blue)  `shouldBe` False
  it "(Red /= Blue) == True"  $ (Red /= Blue)  `shouldBe` True
  it "(Green == Green) == True" $ (Green == Green) `shouldBe` True
