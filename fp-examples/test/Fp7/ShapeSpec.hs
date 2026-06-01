-- | fp6.md 「直和型」節の練習問題 `Shape`.
module Fp7.ShapeSpec (spec) where

import Test.Hspec

data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
           deriving (Show, Eq)

area :: Shape -> Double
area (Circle r)       = pi * r * r
area (Rectangle w h)  = w * h
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

spec :: Spec
spec = describe "Fp7.Shape" $ do
  it "area (Circle 1) == pi" $
    area (Circle 1) `shouldSatisfy` approxEq pi
  it "area (Rectangle 3 4) == 12.0" $
    area (Rectangle 3 4) `shouldBe` 12.0
  it "area (Triangle 3 4 5) ≈ 6.0 (ヘロンの公式 / 3-4-5 直角三角形)" $
    area (Triangle 3 4 5) `shouldSatisfy` approxEq 6.0
  it "area (Triangle 5 5 6) ≈ 12.0 (二等辺三角形)" $
    area (Triangle 5 5 6) `shouldSatisfy` approxEq 12.0
