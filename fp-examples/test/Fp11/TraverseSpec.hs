-- | fp11.md 「構造を走査する」の部 (traverse / 木の走査 / Traversable クラス) のコード例.
--   リストと自作 Tree に対する traverse (Maybe の一括検証, State の連番付け) を検証する.
module Fp11.TraverseSpec (spec) where

import Test.Hspec
import Control.Monad (forM)

-- 自作 State (「インスタンスを自分の手で書く」の定義)
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap g m = State $ \s ->
    let (x, s') = runState m s
    in  (g x, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  mf <*> mx = State $ \s ->
    let (g, s1) = runState mf s
        (x, s2) = runState mx s1
    in  (g x, s2)

instance Monad (State s) where
  m >>= f = State $ \s ->
    let (x, s1) = runState m s
    in  runState (f x) s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  pure n

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

halve :: Int -> Maybe Int
halve n = if even n then Just (n `div` 2) else Nothing

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

labelTree :: Tree String -> Tree (Int, String)
labelTree t = evalState (traverseTree step t) 0
  where
    step x = do
      n <- fresh
      pure (n, x)

spec :: Spec
spec = describe "Fp11.Traverse (構造を走査する)" $ do
  describe "リストの traverse / sequenceA" $ do
    it "traverse halve [2,4,6] == Just [1,2,3]" $
      traverse halve [2, 4, 6] `shouldBe` Just [1, 2, 3]
    it "traverse halve [2,3,6] == Nothing (1 つの失敗で全体が失敗)" $
      traverse halve [2, 3, 6] `shouldBe` Nothing
    it "sequenceA は構造とパターンの入れ替え" $ do
      sequenceA [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3 :: Int]
      sequenceA [Just 1, Nothing, Just 3] `shouldBe` (Nothing :: Maybe [Int])
    it "sequenceA = traverse id" $
      sequenceA [Just 1, Just (2 :: Int)] `shouldBe` traverse id [Just 1, Just 2]
    it "forM は traverse の引数順違い (Maybe を流して同じ結果)" $
      forM [2, 4, 6] halve `shouldBe` traverse halve [2, 4, 6]

  describe "木の走査 (traverseTree)" $ do
    it "全要素が成功すれば木の形を保って返す" $
      traverseTree halve (Node (Node Leaf 2 Leaf) 4 (Node Leaf 6 Leaf))
        `shouldBe` Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
    it "1 つでも失敗すれば木ごと失敗する" $
      traverseTree halve (Node (Node Leaf 2 Leaf) 3 (Node Leaf 6 Leaf))
        `shouldBe` Nothing

  describe "State を流す (連番ラベル付けの一般化)" $
    it "labelTree は左部分木 → 節 → 右部分木の順に番号を振る" $
      labelTree (Node (Node Leaf "a" Leaf) "b" (Node Leaf "c" Leaf))
        `shouldBe` Node (Node Leaf (0, "a") Leaf) (1, "b") (Node Leaf (2, "c") Leaf)

  describe "Traversable クラス (Foldable の恩恵つき)" $ do
    it "クラス経由の traverse も手作りと同じ" $
      traverse halve (Node (Node Leaf 2 Leaf) 4 (Node Leaf (6 :: Int) Leaf))
        `shouldBe` Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
    it "sum / length が木にそのまま使える" $ do
      let t = Node (Node Leaf 2 Leaf) 4 (Node Leaf (6 :: Int) Leaf)
      sum t `shouldBe` 12
      length t `shouldBe` 3
