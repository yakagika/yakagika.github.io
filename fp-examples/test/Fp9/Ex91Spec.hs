-- | fp9.md Exercise CH9-1 「安全な探索関数 safeLast / lookupKey」.
module Fp9.Ex91Spec (spec) where

import Test.Hspec

safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast [x]      = Just x
safeLast (_ : xs) = safeLast xs

lookupKey :: Eq k => k -> [(k, v)] -> Maybe v
lookupKey _ [] = Nothing
lookupKey key ((k, v) : rest)
  | key == k  = Just v
  | otherwise = lookupKey key rest

spec :: Spec
spec = describe "Fp9.Exercise CH9-1" $ do
  it "safeLast [1,2,3] == Just 3" $
    safeLast [1, 2, 3 :: Int] `shouldBe` Just 3
  it "safeLast [] == Nothing" $
    safeLast ([] :: [Int]) `shouldBe` Nothing
  it "lookupKey \"b\" [(\"a\",1),(\"b\",2)] == Just 2" $
    lookupKey "b" [("a", 1), ("b", 2 :: Int)] `shouldBe` Just 2
  it "lookupKey \"z\" [(\"a\",1),(\"b\",2)] == Nothing" $
    lookupKey "z" [("a", 1), ("b", 2 :: Int)] `shouldBe` Nothing
