-- | fp9.md 「Functor (関手) と fmap」節のコード例.
module Fp9.TreeFunctorSpec (spec) where

import Test.Hspec

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 木の形は保ったまま, 各ノードの値に関数を適用する
instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 中間順走査で値を取り出す (確認用)
toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

sample :: Tree Int
sample = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

spec :: Spec
spec = describe "Fp9.TreeFunctor" $ do
  describe "Maybe / リスト / Either の fmap" $ do
    it "fmap (+1) (Just 3) == Just 4" $
      fmap (+ 1) (Just 3) `shouldBe` Just (4 :: Int)
    it "fmap (+1) Nothing == Nothing" $
      fmap (+ 1) (Nothing :: Maybe Int) `shouldBe` Nothing
    it "fmap (*2) [1,2,3] == [2,4,6] (= map)" $
      fmap (* 2) [1, 2, 3] `shouldBe` [2, 4, 6 :: Int]
    it "fmap = map で一致する" $
      fmap (* 2) [1, 2, 3] `shouldBe` map (* 2) [1, 2, 3 :: Int]
    it "fmap (+1) (Right 3) == Right 4" $
      fmap (+ 1) (Right 3 :: Either String Int) `shouldBe` Right 4
    it "fmap (+1) (Left err) == Left err" $
      fmap (+ 1) (Left "err" :: Either String Int) `shouldBe` Left "err"
  describe "Tree の Functor" $ do
    it "toList' sample == [1,2,3]" $
      toList' sample `shouldBe` [1, 2, 3]
    it "toList' (fmap (*10) sample) == [10,20,30]" $
      toList' (fmap (* 10) sample) `shouldBe` [10, 20, 30]
    it "関手則: fmap id sample は構造を保つ (toList' で一致)" $
      toList' (fmap id sample) `shouldBe` toList' sample
