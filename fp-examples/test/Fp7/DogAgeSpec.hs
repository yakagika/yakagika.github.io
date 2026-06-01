-- | fp6.md 「直積型」節 + レコード構文の `DogAge`.
module Fp7.DogAgeSpec (spec) where

import Test.Hspec

data MyDogs = GoldenRetriever | BlackRetriever | ShetlandSheepdog
            | StandardPoodle | Beagle
            deriving (Show, Eq)

-- レコード構文版 DogAge (アクセサ breed, age が自動生成)
data DogAge = MkDogAge { breed :: MyDogs, age :: Int }
            deriving (Show, Eq)

-- 同等の手書きアクセサ (アクセサ関数の説明に対応)
breedManual :: DogAge -> MyDogs
breedManual (MkDogAge b _) = b

-- レコード更新例
olderGolden :: DogAge -> DogAge
olderGolden d = d { age = 10 }

-- 高階関数とアクセサ関数の組み合わせ (fp6.md の説明事例)
dogs :: [DogAge]
dogs = [ MkDogAge GoldenRetriever 3
       , MkDogAge Beagle 7
       , MkDogAge StandardPoodle 5
       ]

allBreeds :: [MyDogs]
allBreeds = map breed dogs

matureDogs :: [DogAge]
matureDogs = filter ((>= 5) . age) dogs

showBreed :: DogAge -> String
showBreed = show . breed

spec :: Spec
spec = describe "Fp7.DogAge" $ do
  let golden = MkDogAge GoldenRetriever 3

  describe "基礎" $ do
    it "breed goldenAge == GoldenRetriever" $
      breed golden `shouldBe` GoldenRetriever
    it "age goldenAge == 3" $
      age golden `shouldBe` 3
    it "手書きアクセサと自動生成アクセサが一致" $
      breedManual golden `shouldBe` breed golden

  describe "レコード更新" $
    it "age を 10 に更新, breed は維持" $ do
      let updated = olderGolden golden
      age updated `shouldBe` 10
      breed updated `shouldBe` GoldenRetriever

  describe "高階関数とアクセサ関数" $ do
    it "map breed dogs == [GoldenRetriever, Beagle, StandardPoodle]" $
      allBreeds `shouldBe` [GoldenRetriever, Beagle, StandardPoodle]
    it "filter ((>=5).age) dogs は Beagle 7 と StandardPoodle 5" $
      matureDogs `shouldBe`
        [MkDogAge Beagle 7, MkDogAge StandardPoodle 5]
    it "showBreed = show . breed が動く" $
      showBreed golden `shouldBe` "GoldenRetriever"
