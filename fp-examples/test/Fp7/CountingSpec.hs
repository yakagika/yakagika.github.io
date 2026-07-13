-- | fp7.md 直積型・直和型の「数え上げ — 「代数的」の意味」節.
--   |A×B| = |A|・|B|, |A+B| = |A| + |B|.
module Fp7.CountingSpec (spec) where

import Test.Hspec

data MyDogs = GoldenRetriever
            | BlackRetriever
            | ShetlandSheepdog
            | StandardPoodle
            | Beagle
            deriving Show

data Size = Small | Medium | Large deriving Show

allDogs :: [MyDogs]
allDogs = [GoldenRetriever, BlackRetriever, ShetlandSheepdog, StandardPoodle, Beagle]

allSizes :: [Size]
allSizes = [Small, Medium, Large]

-- MyDogs × Size の全要素
allPairs :: [(MyDogs, Size)]
allPairs = [ (d, s) | d <- allDogs, s <- allSizes ]

data DogOrSize = ADog  MyDogs
               | ASize Size
               deriving Show

-- DogOrSize の全要素: タグ付きで合流させる
allDogOrSize :: [DogOrSize]
allDogOrSize = [ ADog d | d <- allDogs ] ++ [ ASize s | s <- allSizes ]

spec :: Spec
spec = describe "Fp7.Counting" $ do
  describe "直積の数え上げ |A×B| = |A|・|B|" $ do
    it "|MyDogs| == 5"       $ length allDogs `shouldBe` 5
    it "|Size| == 3"         $ length allSizes `shouldBe` 3
    it "|MyDogs × Size| == 15" $ length allPairs `shouldBe` 15
    it "掛け算と一致する"      $
      length allPairs `shouldBe` length allDogs * length allSizes

  describe "直和の数え上げ |A+B| = |A| + |B|" $ do
    it "|DogOrSize| == 8" $ length allDogOrSize `shouldBe` 8
    it "足し算と一致する"   $
      length allDogOrSize `shouldBe` length allDogs + length allSizes
