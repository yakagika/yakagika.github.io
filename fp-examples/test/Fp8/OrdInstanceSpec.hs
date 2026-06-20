-- | fp8.md 「Ord — 順序」節のコード例.
module Fp8.OrdInstanceSpec (spec) where

import Test.Hspec

data Size = Small | Medium | Large deriving (Eq, Show)

instance Ord Size where
  compare a b = compare (rank a) (rank b)
    where
      rank :: Size -> Int
      rank Small  = 0
      rank Medium = 1
      rank Large  = 2

spec :: Spec
spec = describe "Fp8.OrdInstance" $ do
  it "compare Small Large == LT"             $ compare Small Large             `shouldBe` LT
  it "compare Large Small == GT"             $ compare Large Small             `shouldBe` GT
  it "(Medium < Large) == True"              $ (Medium < Large)                `shouldBe` True
  it "maximum [Small,Large,Medium] == Large" $ maximum [Small, Large, Medium]  `shouldBe` Large
  it "minimum [Small,Large,Medium] == Small" $ minimum [Small, Large, Medium]  `shouldBe` Small
