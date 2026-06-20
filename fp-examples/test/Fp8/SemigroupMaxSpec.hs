-- | fp8.md 「半群 (Semigroup)」節のコード例.
module Fp8.SemigroupMaxSpec (spec) where

import Test.Hspec

newtype Max = Max Int deriving (Show, Eq)

-- (|+|) が数式の ⊔ (= max) に対応する
(|+|) :: Max -> Max -> Max
Max a |+| Max b = Max (max a b)

-- <> は (|+|) そのものとして定義する
instance Semigroup Max where
  (<>) = (|+|)

spec :: Spec
spec = describe "Fp8.SemigroupMax" $ do
  it "Max 3 |+| Max 7 == Max 7"         $ (Max 3 |+| Max 7) `shouldBe` Max 7
  it "Max 3 <> Max 7 == Max 7 (<>=|+|)" $ (Max 3 <> Max 7) `shouldBe` Max 7
  it "結合律 (左結合) == Max 7"           $ ((Max 3 <> Max 7) <> Max 5) `shouldBe` Max 7
  it "結合律 (右結合) == Max 7"           $ (Max 3 <> (Max 7 <> Max 5)) `shouldBe` Max 7
