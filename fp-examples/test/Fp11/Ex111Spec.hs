-- | fp11.md Exercise CH11-1「手書きの配管から State へ」の回答例.
--   仕様: runState pairFresh s = ((s, s + 1), s + 2).
module Fp11.Ex111Spec (spec) where

import Test.Hspec

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

pairFresh0 :: Int -> ((Int, Int), Int)
pairFresh0 s0 =
  let (a, s1) = tick s0
      (b, s2) = tick s1
  in  ((a, b), s2)
  where
    tick n = (n, n + 1)

pairFresh :: State Int (Int, Int)
pairFresh = do
  a <- fresh
  b <- fresh
  pure (a, b)

spec :: Spec
spec = describe "Fp11.Ex111 (Exercise CH11-1: 手書きの配管から State へ)" $ do
  it "手書き版: pairFresh0 0 == ((0,1),2), pairFresh0 10 == ((10,11),12)" $ do
    pairFresh0 0 `shouldBe` ((0, 1), 2)
    pairFresh0 10 `shouldBe` ((10, 11), 12)
  it "State 版も仕様 runState pairFresh s = ((s, s+1), s+2) を満たす" $ do
    runState pairFresh 0 `shouldBe` ((0, 1), 2)
    runState pairFresh 10 `shouldBe` ((10, 11), 12)
  it "2 つの実装は任意の初期値で一致する" $
    map (runState pairFresh) [0, 1, 7, 100] `shouldBe` map pairFresh0 [0, 1, 7, 100]
