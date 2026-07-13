-- | fp9v2.md 「データ型は対象, 関数は射 — 圏 Hask」節のコード例.
module Fp9V2.CategorySpec (spec) where

import Test.Hspec

-- 射 = 関数, 合成 = (.), 恒等射 = id
f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

-- 対象が相異なる合成の例 (可換図式): Bool -> Int -> String
showBit :: Bool -> String
showBit = show . fromEnum

spec :: Spec
spec = describe "Fp9V2.Category (Hask)" $ do
  it "(g . f) 3 == 8" $
    (g . f) 3 `shouldBe` 8
  it "id 3 == 3" $
    id 3 `shouldBe` (3 :: Int)
  it "(f . id) 3 == 4 (恒等律)" $
    (f . id) 3 `shouldBe` 4
  it "結合律: (g . f) . id == g . (f . id)" $
    (((g . f) . id) 3) `shouldBe` ((g . (f . id)) 3)
  it "fromEnum True == 1" $
    fromEnum True `shouldBe` (1 :: Int)
  it "showBit True == \"1\" (show . fromEnum)" $
    showBit True `shouldBe` "1"
  it "showBit False == \"0\"" $
    showBit False `shouldBe` "0"
