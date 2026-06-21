-- | fp8.md 演習 CH8-4 の回答例.
module Fp8.MedalSpec (spec) where

import Test.Hspec

data Medal = Gold | Silver | Bronze deriving Show

instance Eq Medal where
  Gold   == Gold   = True
  Silver == Silver = True
  Bronze == Bronze = True
  _      == _      = False

instance Ord Medal where
  compare a b = compare (rank b) (rank a)
    where
      rank :: Medal -> Int
      rank Gold   = 0
      rank Silver = 1
      rank Bronze = 2

spec :: Spec
spec = describe "Fp8.Medal" $ do
  it "(Gold == Gold) == True"   $ (Gold == Gold)                  `shouldBe` True
  it "(Gold == Silver) == False" $ (Gold == Silver)               `shouldBe` False
  it "compare Gold Bronze == GT" $ compare Gold Bronze            `shouldBe` GT
  it "compare Bronze Gold == LT" $ compare Bronze Gold            `shouldBe` LT
  it "(Silver < Gold) == True"  $ (Silver < Gold)                 `shouldBe` True
  it "maximum [Bronze,Gold,Silver] == Gold" $
    maximum [Bronze, Gold, Silver] `shouldBe` Gold
  it "minimum [Bronze,Gold,Silver] == Bronze" $
    minimum [Bronze, Gold, Silver] `shouldBe` Bronze
