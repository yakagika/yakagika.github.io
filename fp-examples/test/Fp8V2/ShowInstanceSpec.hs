-- | fp8v2.md 「Show — deriving の中身を手で書く」節のコード例.
module Fp8V2.ShowInstanceSpec (spec) where

import Test.Hspec

data Color = Red | Green | Blue

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

spec :: Spec
spec = describe "Fp8V2.ShowInstance" $ do
  it "show Red == \"Red\""   $ show Red   `shouldBe` "Red"
  it "show Green == \"Green\"" $ show Green `shouldBe` "Green"
  it "show Blue == \"Blue\""  $ show Blue  `shouldBe` "Blue"
