-- | fp11.md Exercise CH11-2「コイン投げを State で」の回答例.
--   仕様: runState coin s = (even (lcgStep s), lcgStep s).
module Fp11.Ex112Spec (spec) where

import Test.Hspec
import Control.Monad (replicateM)

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

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

lcgStep :: Int -> Int
lcgStep x = (1103515245 * x + 12345) `mod` 2147483648

coin :: State Int Bool
coin = do
  modify lcgStep
  s <- get
  pure (even s)

flips :: Int -> Int -> [Bool]
flips seed n = evalState (replicateM n coin) seed

countHeads :: Int -> Int -> Int
countHeads seed n = length (filter id (flips seed n))

spec :: Spec
spec = describe "Fp11.Ex112 (Exercise CH11-2: コイン投げを State で)" $ do
  it "仕様の等式: runState coin s = (even (lcgStep s), lcgStep s)" $ do
    let check s = runState coin s == (even (lcgStep s), lcgStep s)
    all check [0, 1, 42, 2026] `shouldBe` True
  it "flips 2026 5 == [False,True,False,True,False]" $
    flips 2026 5 `shouldBe` [False, True, False, True, False]
  it "flips 7 5 == [True,False,True,False,True]" $
    flips 7 5 `shouldBe` [True, False, True, False, True]
  it "countHeads 2026 10 == 5" $
    countHeads 2026 10 `shouldBe` 5
