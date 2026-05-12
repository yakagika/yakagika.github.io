-- | fp4.md 「関数合成」節.
module Fp4.CompositionSpec (spec) where

import Test.Hspec

f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g x = 3 + x

h :: Int -> Int
h = f . g

spec :: Spec
spec = describe "Fp4.Composition" $ do
  it "f (g 2) == 10" $ f (g 2) `shouldBe` 10
  it "(f . g) 2 == 10" $ (f . g) 2 `shouldBe` 10
  it "h 2 == 10 (h = f . g)" $ h 2 `shouldBe` 10
  it "h 0 == 6 (h(0) = f(g(0)) = f(3) = 6)" $ h 0 `shouldBe` 6
