-- | fp11.md Exercise CH11-5「traverse による一括検証」の回答例.
--   仕様: 全要素が 0..100 のときだけ checkAll xs = Just xs.
module Fp11.Ex115Spec (spec) where

import Test.Hspec

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

traverseTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTree _ Leaf         = pure Leaf
traverseTree g (Node l x r) =
  Node <$> traverseTree g l <*> g x <*> traverseTree g r

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
  foldr _ z Leaf         = z
  foldr g z (Node l x r) = foldr g (g x (foldr g z r)) l

instance Traversable Tree where
  traverse = traverseTree

checkScore :: Int -> Maybe Int
checkScore x = if 0 <= x && x <= 100 then Just x else Nothing

checkAll :: [Int] -> Maybe [Int]
checkAll = traverse checkScore

checkTree :: Tree Int -> Maybe (Tree Int)
checkTree = traverse checkScore

spec :: Spec
spec = describe "Fp11.Ex115 (Exercise CH11-5: traverse による一括検証)" $ do
  it "全要素が有効なら Just でそのまま返す" $
    checkAll [80, 95, 60] `shouldBe` Just [80, 95, 60]
  it "1 つでも範囲外なら Nothing" $ do
    checkAll [80, 120, 60] `shouldBe` Nothing
    checkAll [-1] `shouldBe` Nothing
  it "境界値 0 と 100 は有効" $
    checkAll [0, 100] `shouldBe` Just [0, 100]
  it "木でも同じ検証が流せる" $ do
    checkTree (Node Leaf 50 (Node Leaf 100 Leaf))
      `shouldBe` Just (Node Leaf 50 (Node Leaf 100 Leaf))
    checkTree (Node Leaf 50 (Node Leaf 101 Leaf)) `shouldBe` Nothing
