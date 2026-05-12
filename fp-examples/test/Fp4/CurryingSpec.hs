-- | fp4.md 「カリー化, 部分適用」節.
module Fp4.CurryingSpec (spec) where

import Test.Hspec

add :: Int -> Int -> Int
add x y = x + y

-- 部分適用
add5 :: Int -> Int
add5 = add 5

spec :: Spec
spec = describe "Fp4.Currying" $ do
  it "add 5 10 == 15" $ add 5 10 `shouldBe` 15
  it "add5 10 == 15 (部分適用)" $ add5 10 `shouldBe` 15
  it "(add 5) 10 == add 5 10" $ (add 5) 10 `shouldBe` add 5 10
