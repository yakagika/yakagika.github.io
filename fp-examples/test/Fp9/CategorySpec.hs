-- | fp9.md 「データ型は対象, 関数は射」節のコード例.
module Fp9.CategorySpec (spec) where

import Test.Hspec

-- 射 = 関数, 合成 = (.), 恒等射 = id
f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

spec :: Spec
spec = describe "Fp9.Category (Hask)" $ do
  it "(g . f) 3 == 8" $
    (g . f) 3 `shouldBe` 8
  it "id 3 == 3" $
    id 3 `shouldBe` (3 :: Int)
  it "(f . id) 3 == 4 (恒等律)" $
    (f . id) 3 `shouldBe` 4
  it "結合律: (g . f) . id == g . (f . id)" $
    (((g . f) . id) 3) `shouldBe` ((g . (f . id)) 3)
