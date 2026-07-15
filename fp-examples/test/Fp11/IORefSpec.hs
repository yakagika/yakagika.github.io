-- | fp11.md 「IORef」節のコード例.
--   IO の中の可変セル (newIORef / readIORef / modifyIORef) の実行結果を確認する.
module Fp11.IORefSpec (spec) where

import Test.Hspec
import Data.IORef
import Control.Monad (forM_)

sumWithRef :: [Int] -> IO Int
sumWithRef xs = do
  ref <- newIORef 0
  forM_ xs $ \x ->
    modifyIORef ref (+ x)
  readIORef ref

spec :: Spec
spec = describe "Fp11.IORef (IO の中の可変セル)" $ do
  it "newIORef 0 → modifyIORef (+1) ×2 → readIORef == 2" $ do
    ref <- newIORef (0 :: Int)
    modifyIORef ref (+ 1)
    modifyIORef ref (+ 1)
    n <- readIORef ref
    n `shouldBe` 2
  it "writeIORef で上書きできる" $ do
    ref <- newIORef (0 :: Int)
    writeIORef ref 42
    n <- readIORef ref
    n `shouldBe` 42
  it "sumWithRef [1 .. 100] == 5050" $ do
    total <- sumWithRef [1 .. 100]
    total `shouldBe` 5050
