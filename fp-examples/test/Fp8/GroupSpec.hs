-- | fp8.md 「群 (Group)」節のコード例.
module Fp8.GroupSpec (spec) where

import Test.Hspec

class Monoid a => Group a where
  invert :: a -> a

data Z3 = Z0 | Z1 | Z2 deriving (Show, Eq, Enum, Bounded)

-- 群の演算 ⊕:  a ⊕ b = (a + b) mod 3
(.+.) :: Z3 -> Z3 -> Z3
a .+. b = toEnum ((fromEnum a + fromEnum b) `mod` 3)

instance Semigroup Z3 where (<>)   = (.+.)
instance Monoid    Z3 where mempty = Z0
instance Group     Z3 where
  invert a = toEnum ((3 - fromEnum a) `mod` 3)

spec :: Spec
spec = describe "Fp8.Group" $ do
  it "Z1 .+. Z2 == Z0"           $ (Z1 .+. Z2) `shouldBe` Z0
  it "Z2 .+. Z2 == Z1"           $ (Z2 .+. Z2) `shouldBe` Z1
  it "Z1 <> Z2 == Z0 (<> = .+.)" $ (Z1 <> Z2) `shouldBe` Z0
  it "invert Z1 == Z2"           $ invert Z1 `shouldBe` Z2
  it "invert Z2 == Z1"           $ invert Z2 `shouldBe` Z1
  it "invert Z0 == Z0"           $ invert Z0 `shouldBe` Z0
  it "Z1 <> invert Z1 == Z0 (逆元律)" $ (Z1 <> invert Z1) `shouldBe` Z0
  it "mempty :: Z3 == Z0"        $ (mempty :: Z3) `shouldBe` Z0
