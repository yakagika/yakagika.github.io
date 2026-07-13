-- | fp7v2.md 「関数 — 特別な関係」節の graphOn (関数のグラフ).
module Fp7V2.GraphSpec (spec) where

import Test.Hspec

square :: Int -> Int
square x = x * x

-- xs の範囲での f のグラフ {(x, f x) | x ∈ xs}
graphOn :: [a] -> (a -> b) -> [(a, b)]
graphOn xs f = [ (x, f x) | x <- xs ]

spec :: Spec
spec = describe "Fp7V2.Graph" $ do
  it "graphOn [1..5] square (fp7v2.md の期待値)" $
    graphOn [1 .. 5] square `shouldBe`
      [(1, 1), (2, 4), (3, 9), (4, 16), (5, 25)]
  it "グラフの右一意性: 入力ごとに出力は 1 つ (対の数 = 入力の数)" $
    length (graphOn [1 .. 5] square) `shouldBe` 5
