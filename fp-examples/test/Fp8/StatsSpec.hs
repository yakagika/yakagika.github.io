-- | fp8.md 演習 CH8-6 (Stats) と「結合律と準同型が可能にすること」節の例.
--   stats の準同型則, および融合 (中間リスト除去・単一走査・Moments による平均/分散) を検証する.
module Fp8.StatsSpec (spec) where

import Test.Hspec

data Stats = Stats { statCount :: Int, statSum :: Int }
  deriving (Show, Eq)

instance Semigroup Stats where
  Stats c1 s1 <> Stats c2 s2 = Stats (c1 + c2) (s1 + s2)

instance Monoid Stats where
  mempty = Stats 0 0

singleton :: Int -> Stats
singleton n = Stats 1 n

stats :: [Int] -> Stats
stats = mconcat . map singleton

-- 件数・合計・二乗和を 1 つのモノイドに束ね, 平均も分散も 1 パスで出す
data Moments = Moments { mN :: Int, mSum :: Double, mSumSq :: Double }
  deriving (Show, Eq)

instance Semigroup Moments where
  Moments n1 s1 q1 <> Moments n2 s2 q2 = Moments (n1 + n2) (s1 + s2) (q1 + q2)

instance Monoid Moments where
  mempty = Moments 0 0 0

moment :: Double -> Moments
moment x = Moments 1 x (x * x)

mean :: Moments -> Double
mean (Moments n s _) = s / fromIntegral n

variance :: Moments -> Double
variance m@(Moments n _ q) = q / fromIntegral n - mu * mu
  where mu = mean m

spec :: Spec
spec = describe "Fp8.Stats" $ do
  it "singleton 10 <> singleton 20 == Stats 2 30" $
    singleton 10 <> singleton 20 `shouldBe` Stats 2 30
  it "mconcat (map singleton [1,2,3,4]) == Stats 4 10" $
    mconcat (map singleton [1,2,3,4]) `shouldBe` Stats 4 10
  it "mconcat (map singleton []) == Stats 0 0" $
    mconcat (map singleton []) `shouldBe` Stats 0 0
  it "(mempty :: Stats) == Stats 0 0" $
    (mempty :: Stats) `shouldBe` Stats 0 0
  it "stats [1,2,3,4] == Stats 4 10" $
    stats [1, 2, 3, 4] `shouldBe` Stats 4 10

  describe "準同型: 関数適用と合算が交換する" $ do
    it "stats (xs ++ ys) == stats xs <> stats ys" $
      stats ([1, 2] ++ [3, 4]) `shouldBe` stats [1, 2] <> stats [3, 4]
    it "stats [] == mempty" $
      stats [] `shouldBe` (mempty :: Stats)

  describe "融合 (fusion) — 中間リスト除去と単一走査" $ do
    it "中間リスト除去: foldMap singleton == mconcat . map singleton" $
      foldMap singleton [1, 2, 3, 4] `shouldBe` stats [1, 2, 3, 4]
    it "単一走査で (件数, 合計) == (length, sum)" $ do
      let xs = [3, 1, 4, 1, 5, 9, 2, 6] :: [Int]
      (statCount (stats xs), statSum (stats xs)) `shouldBe` (length xs, sum xs)
    it "単一走査で平均・分散 (Moments)" $ do
      let m = foldMap moment [2, 4, 4, 4, 5, 5, 7, 9]
      mean m     `shouldSatisfy` (\v -> abs (v - 5) < 1e-9)
      variance m `shouldSatisfy` (\v -> abs (v - 4) < 1e-9)
