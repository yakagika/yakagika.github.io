-- | fp12.md 選択課題E「Queue — 観測的等価と実装非依存」.
--   表現が本質的に異なる 2 モデル (SimpleQueue / TwoListQueue) が
--   同じ FIFO 公理を満たし, 観測列 (toListQ) で一致することを検証する.
module Fp12.QueueSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- シグネチャ
class QueueSig t where
  emptyQ   :: t a
  enqueue  :: a -> t a -> t a   -- 構築子: 末尾に並ぶ
  front    :: t a -> a          -- 観測子: 先頭を見る
  dequeue  :: t a -> t a        -- 派生:   先頭が抜ける
  isEmptyQ :: t a -> Bool       -- 観測子: 空か

-- モデル 1: 素朴 (末尾へ ++)
newtype SimpleQueue a = SimpleQueue [a] deriving (Show, Eq)

instance QueueSig SimpleQueue where
  emptyQ                         = SimpleQueue []
  enqueue x (SimpleQueue xs)     = SimpleQueue (xs ++ [x])
  front (SimpleQueue (x : _))    = x
  front (SimpleQueue [])         = error "front: empty queue"
  dequeue (SimpleQueue (_ : xs)) = SimpleQueue xs
  dequeue (SimpleQueue [])       = error "dequeue: empty queue"
  isEmptyQ (SimpleQueue xs)      = null xs

-- モデル 2: 前列と後列 (enqueue が O(1))
data TwoListQueue a = TwoListQueue [a] [a] deriving Show

-- 不変条件: 前列が空なら後列も空
fixup :: [a] -> [a] -> TwoListQueue a
fixup [] back = TwoListQueue (reverse back) []
fixup f  back = TwoListQueue f back

instance QueueSig TwoListQueue where
  emptyQ                            = TwoListQueue [] []
  enqueue x (TwoListQueue f b)      = fixup f (x : b)
  front (TwoListQueue (x : _) _)    = x
  front (TwoListQueue [] _)         = error "front: empty queue"
  dequeue (TwoListQueue (_ : f) b)  = fixup f b
  dequeue (TwoListQueue [] _)       = error "dequeue: empty queue"
  isEmptyQ (TwoListQueue f _)       = null f

-- 観測列: 先頭から全部読む
toListQ :: QueueSig t => t a -> [a]
toListQ q
  | isEmptyQ q = []
  | otherwise  = front q : toListQ (dequeue q)

-- 観測的等価: 同じキューが複数の表現を持つので, 観測列で比べる
instance Eq a => Eq (TwoListQueue a) where
  q1 == q2 = toListQ q1 == toListQ q2

instance Arbitrary a => Arbitrary (SimpleQueue a) where
  arbitrary = foldr enqueue emptyQ <$> listOf arbitrary

instance Arbitrary a => Arbitrary (TwoListQueue a) where
  arbitrary = foldr enqueue emptyQ <$> listOf arbitrary

-- 公理 property (どのモデルでも同じ式)
prop_isEmpty_enq :: QueueSig t => Int -> t Int -> Bool
prop_isEmpty_enq x q = not (isEmptyQ (enqueue x q))

prop_front_enq :: QueueSig t => Int -> t Int -> Bool
prop_front_enq x q = front (enqueue x q) == (if isEmptyQ q then x else front q)

prop_deq_enq :: (QueueSig t, Eq (t Int)) => Int -> t Int -> Bool
prop_deq_enq x q =
  dequeue (enqueue x q) == (if isEmptyQ q then emptyQ else enqueue x (dequeue q))

-- モデル間の一致: 同じ操作列 → 同じ観測列
prop_models_agree :: [Int] -> Bool
prop_models_agree xs =
  toListQ (mk xs :: SimpleQueue Int) == toListQ (mk xs :: TwoListQueue Int)
  where
    mk :: QueueSig t => [Int] -> t Int
    mk = foldl (flip enqueue) emptyQ

spec :: Spec
spec = describe "Fp12.Queue (観測的等価と実装非依存)" $ do
  describe "SimpleQueue (素朴なモデル)" $ do
    it "isEmptyQ emptyQ = True" $
      isEmptyQ (emptyQ :: SimpleQueue Int)
    it "FIFO: 1,2,3 を並べて読むと [1,2,3]" $
      toListQ (foldl (flip enqueue) (emptyQ :: SimpleQueue Int) [1, 2, 3])
        `shouldBe` [1, 2, 3]
    prop "isEmptyQ (enqueue x q) = False"
      (prop_isEmpty_enq :: Int -> SimpleQueue Int -> Bool)
    prop "front (enqueue x q) = if isEmptyQ q then x else front q"
      (prop_front_enq :: Int -> SimpleQueue Int -> Bool)
    prop "dequeue (enqueue x q) = if isEmptyQ q then emptyQ else enqueue x (dequeue q)"
      (prop_deq_enq :: Int -> SimpleQueue Int -> Bool)
  describe "TwoListQueue (2 本リストのモデル) — 同じ公理が通る" $ do
    it "isEmptyQ emptyQ = True" $
      isEmptyQ (emptyQ :: TwoListQueue Int)
    it "同じキューの別表現が観測的に等しい: TwoListQueue [1] [2] == TwoListQueue [1,2] []" $
      TwoListQueue [1 :: Int] [2] `shouldBe` TwoListQueue [1, 2] []
    prop "isEmptyQ (enqueue x q) = False"
      (prop_isEmpty_enq :: Int -> TwoListQueue Int -> Bool)
    prop "front (enqueue x q) = if isEmptyQ q then x else front q"
      (prop_front_enq :: Int -> TwoListQueue Int -> Bool)
    prop "dequeue (enqueue x q) = if isEmptyQ q then emptyQ else enqueue x (dequeue q)"
      (prop_deq_enq :: Int -> TwoListQueue Int -> Bool)
  describe "モデル間の一致" $
    prop "同じ操作列を与えると観測列が一致する" prop_models_agree
