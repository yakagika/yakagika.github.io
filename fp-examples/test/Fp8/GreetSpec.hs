-- | fp8.md 「型クラスとは」節の Greet/Animal 例.
module Fp8.GreetSpec (spec) where

import Test.Hspec

class Greet a where
  greet :: a -> String

data Animal = Cat | Dog

instance Greet Animal where
  greet Cat = "にゃー"
  greet Dog = "わん"

hello :: Greet a => a -> String
hello x = "こんにちは, " ++ greet x

spec :: Spec
spec = describe "Fp8.Greet" $ do
  it "greet Cat == \"にゃー\"" $ greet Cat `shouldBe` "にゃー"
  it "greet Dog == \"わん\""   $ greet Dog `shouldBe` "わん"
  it "hello Cat == \"こんにちは, にゃー\"" $
    hello Cat `shouldBe` "こんにちは, にゃー"
