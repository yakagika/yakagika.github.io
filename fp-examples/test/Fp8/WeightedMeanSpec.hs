-- | fp8.md 「半環 (semiring) と加重集計」節のコード例 (加重平均 WMean).
module Fp8.WeightedMeanSpec (spec) where

import Test.Hspec

-- 加重平均: (Σw, Σ w*x) の組を 1 つのモノイドに束ねる
data WMean = WMean { wTotal :: Double, wxTotal :: Double } deriving (Show, Eq)

instance Semigroup WMean where
  WMean w1 wx1 <> WMean w2 wx2 = WMean (w1 + w2) (wx1 + wx2)
instance Monoid WMean where
  mempty = WMean 0 0

-- (重み, 値) を 1 点分に持ち上げる.  w * x に半環の ⊗ (重み付け) が効く
weighted :: (Double, Double) -> WMean
weighted (w, x) = WMean w (w * x)

wmean :: WMean -> Double
wmean (WMean w wx) = wx / w

spec :: Spec
spec = describe "Fp8.WeightedMean (半環と加重集計)" $ do
  describe "加重平均 (weighted average)" $ do
    it "foldMap weighted [(3,4.0),(1,2.0)] == WMean 4.0 14.0" $
      foldMap weighted [(3, 4.0), (1, 2.0)] `shouldBe` WMean 4.0 14.0
    it "wTotal / wxTotal を取り出せる" $ do
      let m = foldMap weighted [(3, 4.0), (1, 2.0)]
      wTotal m `shouldBe` 4.0
      wxTotal m `shouldBe` 14.0
    it "GPA (3単位×4.0 + 1単位×2.0) の加重平均は 3.5" $
      wmean (foldMap weighted [(3, 4.0), (1, 2.0)]) `shouldBe` 3.5
    it "重みが全て 1 なら素の平均に戻る: mean{10,2,6} == 6.0" $
      wmean (foldMap weighted [(1, 10), (1, 2), (1, 6)]) `shouldBe` 6.0
  describe "モノイド則" $ do
    it "mempty は単位元 (右): x <> mempty == x" $
      (WMean 4 14 <> mempty) `shouldBe` WMean 4 14
    it "mempty は単位元 (左): mempty <> x == x" $
      (mempty <> WMean 4 14) `shouldBe` WMean 4 14
    it "空リストは mempty: foldMap weighted [] == WMean 0 0" $
      foldMap weighted [] `shouldBe` (WMean 0 0 :: WMean)
