-- | fp5.md 「直和型」節の `IntOrDog`.
module Fp5.IntOrDogSpec (spec) where

import Test.Hspec

data MyDogs = GoldenRetriever | BlackRetriever | ShetlandSheepdog
            | StandardPoodle | Beagle
            deriving (Show, Eq)

data IntOrDog = MkInt Int
              | MkDog MyDogs
              deriving (Show, Eq)

describe' :: IntOrDog -> String
describe' (MkInt n) = "整数 " ++ show n
describe' (MkDog d) = "犬 " ++ show d

spec :: Spec
spec = describe "Fp5.IntOrDog" $ do
  it "describe (MkInt 42) == \"整数 42\"" $
    describe' (MkInt 42) `shouldBe` "整数 42"
  it "describe (MkDog GoldenRetriever) == \"犬 GoldenRetriever\"" $
    describe' (MkDog GoldenRetriever) `shouldBe` "犬 GoldenRetriever"
