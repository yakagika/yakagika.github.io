-- | fp11.md 「State の正体」「インスタンスを自分の手で書く」「状態を読み書きする部品」
--   「カウンタから疑似乱数まで」節のコード例.
--   自作 State の Functor/Applicative/Monad インスタンスと, get/put/modify/gets,
--   fresh / label / LCG (rolls) を検証する.
module Fp11.StateSpec (spec) where

import Test.Hspec
import Control.Monad (forM, replicateM)

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

-- 「State の正体」の部品 (pureS = pure, joinS = μ)
pureS :: a -> State s a
pureS x = State $ \s -> (x, s)

joinS :: State s (State s a) -> State s a
joinS mm = State $ \s ->
  let (m, s') = runState mm s
  in  runState m s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

tickS :: State Int Int
tickS = State $ \n -> (n, n + 1)

threeS :: State Int (Int, Int, Int)
threeS = do
  a <- tickS
  b <- tickS
  c <- tickS
  pure (a, b, c)

fresh :: State Int Int
fresh = do
  n <- get
  put (n + 1)
  pure n

label :: [String] -> [(Int, String)]
label xs = evalState (forM xs step) 0
  where
    step x = do
      n <- fresh
      pure (n, x)

lcgStep :: Int -> Int
lcgStep x = (1103515245 * x + 12345) `mod` 2147483648

nextRand :: State Int Int
nextRand = do
  modify lcgStep
  get

die :: State Int Int
die = do
  r <- nextRand
  pure (r `mod` 6 + 1)

rolls :: Int -> Int -> [Int]
rolls seed n = evalState (replicateM n die) seed

spec :: Spec
spec = describe "Fp11.State (自作 State モナド)" $ do
  describe "インスタンスと do 記法" $ do
    it "runState threeS 0 == ((0,1,2),3)" $
      runState threeS 0 `shouldBe` ((0, 1, 2), 3)
    it "runState threeS 10 == ((10,11,12),13)" $
      runState threeS 10 `shouldBe` ((10, 11, 12), 13)
    it "fmap は結果側だけを変換し状態は変えない" $
      runState (fmap (* 10) tickS) 3 `shouldBe` (30, 4)
    it "<*> は状態を左から右へ流す" $
      runState (State (\s -> ((+ s), s + 1)) <*> tickS) 0 `shouldBe` (1, 2)

  describe "正体の部品 (η / μ)" $ do
    it "単位律: joinS (pureS m) と m は同じ計算" $
      map (runState (joinS (pureS tickS))) [0, 5] `shouldBe` map (runState tickS) [0, 5]
    it "bind m f = joinS (fmap f m) (第10章の等式)" $ do
      let f n = State $ \s -> (n * 100, s + n)
      runState (joinS (fmap f tickS)) 3 `shouldBe` runState (tickS >>= f) 3

  describe "状態を読み書きする部品" $ do
    it "runState fresh の仕様: (いまの番号, 番号 + 1)" $ do
      runState fresh 0 `shouldBe` (0, 1)
      runState fresh 41 `shouldBe` (41, 42)
    it "gets で状態の一部を読み出せる" $
      runState (gets (* 2)) 21 `shouldBe` (42, 21)
    it "evalState / execState は結果 / 最終状態だけを返す" $ do
      evalState threeS 0 `shouldBe` (0, 1, 2)
      execState threeS 0 `shouldBe` 3

  describe "連番ラベル付け (forM と State)" $
    it "label [apple, orange, grape]" $
      label ["apple", "orange", "grape"]
        `shouldBe` [(0, "apple"), (1, "orange"), (2, "grape")]

  describe "疑似乱数 (LCG)" $ do
    it "rolls 2026 5 == [4,5,4,3,4]" $
      rolls 2026 5 `shouldBe` [4, 5, 4, 3, 4]
    it "同じ種からは同じ列 (再現性)" $
      rolls 2026 5 `shouldBe` rolls 2026 5
    it "rolls 1 5 == [1,4,5,2,5]" $
      rolls 1 5 `shouldBe` [1, 4, 5, 2, 5]
