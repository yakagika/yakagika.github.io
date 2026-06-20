-- | fp9.md 「多相型と自然変換」節のコード例.
module Fp9.NaturalSpec (spec) where

import Test.Hspec

-- リストを Maybe へ: 先頭があれば Just, 空なら Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

-- Maybe をリストへ: Just x は [x], Nothing は []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

spec :: Spec
spec = describe "Fp9.Natural" $ do
  it "listToMaybe [1,2,3] == Just 1" $
    listToMaybe [1, 2, 3 :: Int] `shouldBe` Just 1
  it "listToMaybe [] == Nothing" $
    listToMaybe ([] :: [Int]) `shouldBe` Nothing
  it "maybeToList (Just 5) == [5]" $
    maybeToList (Just 5 :: Maybe Int) `shouldBe` [5]
  it "maybeToList Nothing == []" $
    maybeToList (Nothing :: Maybe Int) `shouldBe` ([] :: [Int])
  it "自然性: fmap (+1) . listToMaybe == listToMaybe . fmap (+1)" $
    (fmap (+ 1) . listToMaybe) [10, 20, 30 :: Int]
      `shouldBe` (listToMaybe . fmap (+ 1)) [10, 20, 30]
