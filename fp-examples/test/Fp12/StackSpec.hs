-- | fp12.md 導入節「仕様を等式で書く」の通し実演 (Stack).
--   シグネチャ = 型クラス, 公理 = QuickCheck property, モデル = ListStack / RevStack.
--   同じ公理 (property) が 2 つのモデルで通ること (実装非依存) を検証する.
module Fp12.StackSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- シグネチャ (fp8 の型クラス)
class StackSig t where
  empty   :: t a                 -- 構築子: 空のスタック
  push    :: a -> t a -> t a     -- 構築子: 1 つ積む
  pop     :: t a -> t a          -- 派生:   1 つ降ろす
  top     :: t a -> a            -- 観測子: 先頭を覗く
  isEmpty :: t a -> Bool         -- 観測子: 空か

-- モデル 1: リストの先頭に積む
newtype ListStack a = ListStack [a] deriving (Show, Eq)

instance StackSig ListStack where
  empty                       = ListStack []
  push x (ListStack xs)       = ListStack (x : xs)
  pop    (ListStack (_ : xs)) = ListStack xs
  pop    (ListStack [])       = error "pop: empty stack"
  top    (ListStack (x : _))  = x
  top    (ListStack [])       = error "top: empty stack"
  isEmpty (ListStack xs)      = null xs

-- モデル 2: リストを逆順に持つ (末尾に積む)
newtype RevStack a = RevStack [a] deriving (Show, Eq)

instance StackSig RevStack where
  empty                 = RevStack []
  push x (RevStack xs)  = RevStack (xs ++ [x])
  pop    (RevStack xs)  = RevStack (init xs)
  top    (RevStack xs)  = last xs
  isEmpty (RevStack xs) = null xs

-- 「任意のスタック」も構築子で作る (ゴミなし)
instance Arbitrary a => Arbitrary (ListStack a) where
  arbitrary = foldr push empty <$> listOf arbitrary

instance Arbitrary a => Arbitrary (RevStack a) where
  arbitrary = foldr push empty <$> listOf arbitrary

-- 公理をそのまま property に (どのモデルでも同じ式)
prop_isEmpty_push :: (StackSig t) => Int -> t Int -> Bool
prop_isEmpty_push x s = not (isEmpty (push x s))

prop_top_push :: (StackSig t) => Int -> t Int -> Bool
prop_top_push x s = top (push x s) == x

prop_pop_push :: (StackSig t, Eq (t Int)) => Int -> t Int -> Bool
prop_pop_push x s = pop (push x s) == s

spec :: Spec
spec = describe "Fp12.Stack (仕様=公理, モデル=2実装)" $ do
  describe "ListStack (先頭に積むモデル)" $ do
    it "isEmpty empty = True" $
      isEmpty (empty :: ListStack Int)
    prop "isEmpty (push x s) = False" (prop_isEmpty_push :: Int -> ListStack Int -> Bool)
    prop "top (push x s) = x" (prop_top_push :: Int -> ListStack Int -> Bool)
    prop "pop (push x s) = s" (prop_pop_push :: Int -> ListStack Int -> Bool)
  describe "RevStack (末尾に積むモデル) — 同じ公理が通る" $ do
    it "isEmpty empty = True" $
      isEmpty (empty :: RevStack Int)
    prop "isEmpty (push x s) = False" (prop_isEmpty_push :: Int -> RevStack Int -> Bool)
    prop "top (push x s) = x" (prop_top_push :: Int -> RevStack Int -> Bool)
    prop "pop (push x s) = s" (prop_pop_push :: Int -> RevStack Int -> Bool)
