-- | fp10.md 「独立な計算の組み合わせ」節のコード例.
--   手作りの both / lift2 / lift3 / bothList と, 標準の liftA2 / <*> との一致を確認する.
module Fp10.BothSpec (spec) where

import Test.Hspec
import Control.Applicative (liftA2)

both :: Maybe a -> Maybe b -> Maybe (a, b)
both (Just x) (Just y) = Just (x, y)
both _        _        = Nothing

lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 g x y = fmap (uncurry g) (both x y)

lift3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
lift3 g x y z = fmap (\((a, b), c) -> g a b c) (both (both x y) z)

bothList :: [a] -> [b] -> [(a, b)]
bothList xs ys = [ (x, y) | x <- xs, y <- ys ]

spec :: Spec
spec = describe "Fp10.Both (独立な計算の組み合わせの手作り)" $ do
  describe "both (Maybe の乗法 μ)" $
    it "両方そろったときだけ組にする" $ do
      both (Just 2) (Just 'a') `shouldBe` Just (2 :: Int, 'a')
      both (Just 2 :: Maybe Int) (Nothing :: Maybe Char) `shouldBe` Nothing
      both (Nothing :: Maybe Int) (Just 'a') `shouldBe` Nothing

  describe "lift2 / lift3 (both と fmap による組み立て)" $ do
    it "lift2 (+) (Just 2) (Just 3) == Just 5, 片方欠けたら Nothing" $ do
      lift2 (+) (Just 2) (Just 3) `shouldBe` Just (5 :: Int)
      lift2 (+) (Just 2) Nothing `shouldBe` (Nothing :: Maybe Int)
    it "lift3 で 3 引数へ畳み込める" $ do
      lift3 (\a b c -> a + b + c) (Just 1) (Just 2) (Just 3) `shouldBe` Just (6 :: Int)
      lift3 (\a b c -> a + b + c) (Just 1) Nothing (Just 3) `shouldBe` (Nothing :: Maybe Int)
    it "lift2 は標準の liftA2 と一致する" $
      lift2 (+) (Just 2) (Just 3) `shouldBe` liftA2 (+) (Just 2) (Just (3 :: Int))

  describe "bothList (リストの乗法 μ = 直積)" $ do
    it "すべての組合せを作る" $
      bothList [1, 2 :: Int] "ab" `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
    it "fmap (uncurry (+)) . bothList は liftA2 (+) と一致する" $
      fmap (uncurry (+)) (bothList [1, 2] [10, 20 :: Int])
        `shouldBe` liftA2 (+) [1, 2] [10, 20]

  describe "<*> の分業読み" $
    it "ax <*> bx = fmap (剥がして適用) (both ax bx)" $ do
      let ax = Just (+ 1)
          bx = Just (3 :: Int)
      fmap (\(g, x) -> g x) (both ax bx) `shouldBe` (ax <*> bx)
