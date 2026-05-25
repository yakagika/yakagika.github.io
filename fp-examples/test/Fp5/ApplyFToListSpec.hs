-- | fp5.md 「高階関数」節の `applyFToList` と map の使用例.
module Fp5.ApplyFToListSpec (spec) where

import Test.Hspec

applyFToList :: (a -> b) -> [a] -> [b]
applyFToList _ []     = []
applyFToList f (x:xs) = f x : applyFToList f xs

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

spec :: Spec
spec = describe "Fp5.ApplyFToList" $ do
  it "applyFToList (2*) [4,5,6] == [8,10,12]" $
    applyFToList (2*) [4 :: Int, 5, 6] `shouldBe` [8, 10, 12]
  it "applyFToList (1+) [4,5,6] == [5,6,7]" $
    applyFToList (1+) [4 :: Int, 5, 6] `shouldBe` [5, 6, 7]
  it "applyFToList show [4,5,6] == [\"4\",\"5\",\"6\"]" $
    applyFToList show [4 :: Int, 5, 6] `shouldBe` ["4", "5", "6"]
  it "applyFToList fib [4,5,6] == [5,8,13]" $
    applyFToList fib [4, 5, 6] `shouldBe` [5, 8, 13]

  -- 同じ結果が組み込みの map でも得られる
  it "map (2*) [4,5,6] == applyFToList (2*) [4,5,6]" $
    map (2*) [4 :: Int, 5, 6] `shouldBe` applyFToList (2*) [4, 5, 6]
