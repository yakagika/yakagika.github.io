-- | fp5.md の filter / fold / zipWith / zip 使用例.
module Fp6.FilterFoldZipSpec (spec) where

import Test.Hspec

-- zip の自前定義例
zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

tuple :: a -> b -> (a, b)
tuple a b = (a, b)

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith tuple

spec :: Spec
spec = describe "Fp6.FilterFoldZip" $ do
  describe "filter" $
    it "filter (10 <) [5,10,15,20] == [15,20]" $
      filter (10 <) [5 :: Int, 10, 15, 20] `shouldBe` [15, 20]

  describe "foldl" $
    it "foldl (+) 0 [1,2,3] == 6" $
      foldl (+) 0 [1 :: Int, 2, 3] `shouldBe` 6

  describe "zipWith / zip" $ do
    it "zipWith (++) [\"a\",\"b\",\"c\"] [\"x\",\"y\",\"z\"] == [\"ax\",\"by\",\"cz\"]" $
      zipWith (++) ["a", "b", "c"] ["x", "y", "z"] `shouldBe` ["ax", "by", "cz"]
    it "zip [1,2,3] [11,12,13] == [(1,11),(2,12),(3,13)]" $
      zip [1 :: Int, 2, 3] [11 :: Int, 12, 13] `shouldBe`
        [(1, 11), (2, 12), (3, 13)]
    it "zip' [1,2,3] [11,12,13] と zip が一致" $
      zip' [1 :: Int, 2, 3] [11 :: Int, 12, 13] `shouldBe`
        zip [1 :: Int, 2, 3] [11 :: Int, 12, 13]
    it "zip'' [1,2,3] [11,12,13] と zip が一致" $
      zip'' [1 :: Int, 2, 3] [11 :: Int, 12, 13] `shouldBe`
        zip [1 :: Int, 2, 3] [11 :: Int, 12, 13]
