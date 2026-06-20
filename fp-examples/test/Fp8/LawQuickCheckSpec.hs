-- | fp8.md 「コラム: 法則を Haskell でどう守るか — QuickCheck」のコード例.
module Fp8.LawQuickCheckSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..))

newtype Add = Add Int deriving (Show, Eq)
instance Semigroup Add where Add a <> Add b = Add (a + b)
instance Monoid    Add where mempty = Add 0

instance Arbitrary Add where
  arbitrary = Add <$> arbitrary

-- 結合律:  ∀ x y z. (x <> y) <> z == x <> (y <> z)
prop_assoc :: Add -> Add -> Add -> Bool
prop_assoc x y z = (x <> y) <> z == x <> (y <> z)

-- 単位元律:  ∀ x. mempty <> x == x  かつ  x <> mempty == x
prop_identity :: Add -> Bool
prop_identity x = mempty <> x == x && x <> mempty == x

spec :: Spec
spec = describe "Fp8.LawQuickCheck (QuickCheck で法則を検証)" $ do
  prop "結合律  (x<>y)<>z == x<>(y<>z)" prop_assoc
  prop "単位元律  mempty<>x == x && x<>mempty == x" prop_identity
