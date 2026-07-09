-- | fp10.md 「Functor の復習」「Applicative — 複数の関手値を組み合わせる」節のコード例.
module Fp10.ApplicativeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp10.Applicative" $ do
  describe "Functor の復習 (fmap は 1 引数関数を持ち上げる)" $ do
    it "fmap (+1) (Just 3) == Just 4" $
      fmap (+ 1) (Just 3) `shouldBe` Just (4 :: Int)
    it "fmap (*2) [1,2,3] == [2,4,6]" $
      fmap (* 2) [1, 2, 3] `shouldBe` [2, 4, 6 :: Int]

  describe "Maybe の Applicative" $ do
    it "Just (+1) <*> Just 3 == Just 4" $
      (Just (+ 1) <*> Just 3) `shouldBe` Just (4 :: Int)
    it "pure (+) <*> Just 2 <*> Just 3 == Just 5" $
      (pure (+) <*> Just 2 <*> Just 3) `shouldBe` Just (5 :: Int)
    it "(+) <$> Just 2 <*> Just 3 == Just 5" $
      ((+) <$> Just 2 <*> Just 3) `shouldBe` Just (5 :: Int)
    it "(+) <$> Just 2 <*> Nothing == Nothing (一方でも欠ければ全体が失敗)" $
      ((+) <$> Just 2 <*> Nothing) `shouldBe` (Nothing :: Maybe Int)

  describe "リストの Applicative (すべての組合せ)" $ do
    it "[(+1),(*10)] <*> [1,2] == [2,3,10,20]" $
      ([(+ 1), (* 10)] <*> [1, 2 :: Int]) `shouldBe` [2, 3, 10, 20]
    it "(,) <$> [1,2] <*> \"ab\" == 直積" $
      ((,) <$> [1, 2 :: Int] <*> "ab")
        `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]

  describe "fmap は Applicative の特別な場合 (fmap f x = pure f <*> x)" $
    it "fmap (+1) (Just 3) == pure (+1) <*> Just 3" $
      fmap (+ 1) (Just 3) `shouldBe` (pure (+ 1) <*> Just 3 :: Maybe Int)
