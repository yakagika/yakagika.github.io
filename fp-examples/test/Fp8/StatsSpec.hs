-- | fp8.md 演習 CH8-6 の回答例.
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
