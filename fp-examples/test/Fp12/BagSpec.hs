-- | fp12.md 選択課題D「Bag と Set — 公理が値を同一視する」.
--   構築子どうしの公理 (可換律・冪等律) と観測的な Eq の設計を検証する.
--   ListBag は可換律を満たすが冪等律は満たさない (expectFailure で確認),
--   ListSet は両方を満たす.
module Fp12.BagSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.List (sort, nub)

-- シグネチャ
class BagSig t where
  emptyBag :: t a
  insert   :: Eq a => a -> t a -> t a
  count    :: Eq a => a -> t a -> Int
  size     :: t a -> Int

-- Bag: リストで持ち, 等しさは「並べ替えて同じ」(観測的 Eq)
newtype ListBag a = ListBag [a] deriving Show

instance Ord a => Eq (ListBag a) where
  ListBag xs == ListBag ys = sort xs == sort ys

instance BagSig ListBag where
  emptyBag              = ListBag []
  insert x (ListBag xs) = ListBag (x : xs)
  count y (ListBag xs)  = length (filter (== y) xs)
  size (ListBag xs)     = length xs

-- Set: 挿入時に重複を捨てる (冪等律を満たすモデル)
newtype ListSet a = ListSet [a] deriving Show

instance Ord a => Eq (ListSet a) where
  ListSet xs == ListSet ys = sort (nub xs) == sort (nub ys)

instance BagSig ListSet where
  emptyBag = ListSet []
  insert x s@(ListSet xs)
    | x `elem` xs = s
    | otherwise   = ListSet (x : xs)
  count y (ListSet xs) = length (filter (== y) xs)
  size (ListSet xs)    = length xs

-- 「任意の Bag/Set」も構築子で作る
instance (Arbitrary a, Eq a) => Arbitrary (ListBag a) where
  arbitrary = foldr insert emptyBag <$> listOf arbitrary

instance (Arbitrary a, Eq a) => Arbitrary (ListSet a) where
  arbitrary = foldr insert emptyBag <$> listOf arbitrary

-- 公理 property
prop_count_empty :: Int -> Bool
prop_count_empty y = count y (emptyBag :: ListBag Int) == 0

prop_count_insert :: Int -> Int -> ListBag Int -> Bool
prop_count_insert y x b =
  count y (insert x b) == (if x == y then 1 else 0) + count y b

prop_size_insert :: Int -> ListBag Int -> Bool
prop_size_insert x b = size (insert x b) == 1 + size b

prop_comm :: (BagSig t, Eq (t Int)) => Int -> Int -> t Int -> Bool
prop_comm x y b = insert x (insert y b) == insert y (insert x b)

prop_idem :: (BagSig t, Eq (t Int)) => Int -> t Int -> Bool
prop_idem x b = insert x (insert x b) == insert x b

spec :: Spec
spec = describe "Fp12.Bag (公理による同一視)" $ do
  describe "ListBag (多重集合)" $ do
    prop "count y emptyBag = 0" prop_count_empty
    prop "count y (insert x b) = (x==y なら 1, 違えば 0) + count y b" prop_count_insert
    prop "size (insert x b) = 1 + size b" prop_size_insert
    prop "可換律: insert x (insert y b) = insert y (insert x b)"
      (prop_comm :: Int -> Int -> ListBag Int -> Bool)
    prop "冪等律は成り立たない (反例が出ることを確認)"
      (expectFailure (prop_idem :: Int -> ListBag Int -> Bool))
  describe "ListSet (冪等律を足したモデル)" $ do
    prop "可換律: insert x (insert y s) = insert y (insert x s)"
      (prop_comm :: Int -> Int -> ListSet Int -> Bool)
    prop "冪等律: insert x (insert x s) = insert x s"
      (prop_idem :: Int -> ListSet Int -> Bool)
