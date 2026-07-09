-- | fp10.md Exercise CH10-2 「do 記法で連想リストを 2 回引く」.
module Fp10.Ex102Spec (spec) where

import Test.Hspec

addLookup :: [(String, Int)] -> Maybe Int
addLookup env = do
  x <- lookup "x" env
  y <- lookup "y" env
  return (x + y)

spec :: Spec
spec = describe "Fp10.Exercise CH10-2" $ do
  it "addLookup [(x,3),(y,4)] == Just 7" $
    addLookup [("x", 3), ("y", 4)] `shouldBe` Just 7
  it "addLookup [(x,3)] == Nothing (y が無い)" $
    addLookup [("x", 3)] `shouldBe` Nothing
  it "addLookup [(y,4)] == Nothing (x が無い)" $
    addLookup [("y", 4)] `shouldBe` Nothing
