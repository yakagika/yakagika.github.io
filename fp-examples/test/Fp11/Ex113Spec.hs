-- | fp11.md Exercise CH11-3「同じ計算を State と ST で」の回答例.
--   仕様: minMax xs = (minimum xs, maximum xs) (xs は空でない).
module Fp11.Ex113Spec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Control.Monad.ST
import Data.STRef

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

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

minMaxState :: [Int] -> (Int, Int)
minMaxState (x : xs) = execState (forM_ xs step) (x, x)
  where
    step y = modify (\(lo, hi) -> (min lo y, max hi y))
minMaxState [] = error "empty list"

minMaxST :: [Int] -> (Int, Int)
minMaxST (x : xs) = runST $ do
  lo <- newSTRef x
  hi <- newSTRef x
  forM_ xs $ \y -> do
    modifySTRef lo (min y)
    modifySTRef hi (max y)
  l <- readSTRef lo
  h <- readSTRef hi
  pure (l, h)
minMaxST [] = error "empty list"

spec :: Spec
spec = describe "Fp11.Ex113 (Exercise CH11-3: minMax を State と ST で)" $ do
  it "minMaxState [3,1,4,1,5,9,2,6] == (1,9)" $
    minMaxState [3, 1, 4, 1, 5, 9, 2, 6] `shouldBe` (1, 9)
  it "minMaxST [3,1,4,1,5,9,2,6] == (1,9)" $
    minMaxST [3, 1, 4, 1, 5, 9, 2, 6] `shouldBe` (1, 9)
  it "どちらも仕様 (minimum xs, maximum xs) を満たす" $ do
    let cases = [[42], [1, 2, 3], [5, 5, 5], [-3, 0, 7, -10]]
    map minMaxState cases `shouldBe` [(minimum xs, maximum xs) | xs <- cases]
    map minMaxST cases `shouldBe` [(minimum xs, maximum xs) | xs <- cases]
