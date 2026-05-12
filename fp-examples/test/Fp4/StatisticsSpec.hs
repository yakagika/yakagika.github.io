-- | fp4.md 「練習問題 (関数総合)」の統計量 (標本標準偏差・積率相関係数).
module Fp4.StatisticsSpec (spec) where

import Test.Hspec

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev xs = sqrt variance
  where
    m = mean xs
    n = fromIntegral (length xs)
    variance = sum (map (\x -> (x - m) ^ (2 :: Int)) xs) / n

correlation :: [Double] -> [Double] -> Double
correlation xs ys = covariance / (stddev xs * stddev ys)
  where
    mx = mean xs
    my = mean ys
    n  = fromIntegral (length xs)
    covariance = sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys) / n

-- 浮動小数の比較は小さな誤差を許容
approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

spec :: Spec
spec = describe "Fp4.Statistics" $ do
  let xs = [1.0 .. 5.0]
      ys = [5.0, 4.0, 3.0, 2.0, 1.0]
  it "mean [1..5] == 3.0" $
    mean xs `shouldBe` 3.0
  it "stddev [1..5] ≈ sqrt 2 (fp4.md の期待値)" $
    stddev xs `shouldSatisfy` approxEq (sqrt 2)
  it "correlation [1..5] [5..1] ≈ -1.0 (fp4.md の期待値)" $
    correlation xs ys `shouldSatisfy` approxEq (-1.0)
