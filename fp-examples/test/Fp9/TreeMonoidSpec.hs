-- | fp9.md 「ツリー」節のコード例.
module Fp9.TreeMonoidSpec (spec) where

import Test.Hspec

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = Node Leaf x Leaf
insert' x t@(Node l y r)
  | x < y     = Node (insert' x l) y r
  | x > y     = Node l y (insert' x r)
  | otherwise = t

toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

fromList' :: Ord a => [a] -> Tree a
fromList' = foldr insert' Leaf

-- 合併演算 ⊔:  t1 ⊔ t2 = 「t1 の全要素を t2 に挿入した木」
(|+|) :: Ord a => Tree a -> Tree a -> Tree a
t1 |+| t2 = foldr insert' t2 (toList' t1)

instance Ord a => Semigroup (Tree a) where (<>)   = (|+|)
instance Ord a => Monoid    (Tree a) where mempty = Leaf

spec :: Spec
spec = describe "Fp9.TreeMonoid" $ do
  it "toList' (fromList' [5,3,8]) == [3,5,8]" $
    toList' (fromList' [5,3,8] :: Tree Int) `shouldBe` [3,5,8]
  it "toList' (t1 |+| t2) == [1,3,5,8,9]" $
    toList' ((fromList' [5,3,8] |+| fromList' [3,9,1]) :: Tree Int) `shouldBe` [1,3,5,8,9]
  it "toList' (t1 <> t2) == [1,3,5,8,9] (<> = |+|)" $
    toList' ((fromList' [5,3,8] <> fromList' [3,9,1]) :: Tree Int) `shouldBe` [1,3,5,8,9]
  it "toList' (mempty :: Tree Int) == []" $
    toList' (mempty :: Tree Int) `shouldBe` ([] :: [Int])
  it "重複は除外される (fromList' [3,3,3]) == [3]" $
    toList' (fromList' [3,3,3] :: Tree Int) `shouldBe` [3]
