-- | fp8v2.md 「群 (Group)」「ℤ/7ℤ — 商集合が群をなす」節のコード例.
module Fp8V2.GroupSpec (spec) where

import Test.Hspec

class Monoid a => Group a where
  invert :: a -> a

newtype Z7 = Z7 Int deriving (Show, Eq)

-- 商への射影: 整数をその同値類の代表元 (0..6) へ送る
mkZ7 :: Int -> Z7
mkZ7 n = Z7 (n `mod` 7)

-- 群の演算 ⊕:  [a] ⊕ [b] = [a + b]
(.@.) :: Z7 -> Z7 -> Z7
Z7 a .@. Z7 b = mkZ7 (a + b)

instance Semigroup Z7 where (<>)   = (.@.)
instance Monoid    Z7 where mempty = mkZ7 0
instance Group     Z7 where
  invert (Z7 a) = mkZ7 (7 - a)

spec :: Spec
spec = describe "Fp8V2.Group (Z7 = ℤ/7ℤ)" $ do
  it "mkZ7 1 ⊕ mkZ7 3 == mkZ7 4 (月曜の 3 日後は木曜)" $
    (mkZ7 1 .@. mkZ7 3) `shouldBe` mkZ7 4
  it "mkZ7 5 ⊕ mkZ7 4 == mkZ7 2 (9 ≡ 2)" $
    (mkZ7 5 .@. mkZ7 4) `shouldBe` mkZ7 2
  it "invert (mkZ7 2) == mkZ7 5" $
    invert (mkZ7 2) `shouldBe` mkZ7 5
  it "元と逆元の演算は単位元" $
    (mkZ7 2 <> invert (mkZ7 2)) `shouldBe` (mempty :: Z7)
  it "mkZ7 9 == mkZ7 2 (同じ同値類は代表元に正規化される)" $
    mkZ7 9 `shouldBe` mkZ7 2
  it "逆元律の全数検査 (代表元 0..6)" $
    and [ (mkZ7 a <> invert (mkZ7 a)) == mempty | a <- [0..6] ]
      `shouldBe` True
  it "結合律の全数検査 (代表元 0..6 の全 3 つ組)" $
    and [ (mkZ7 a <> mkZ7 b) <> mkZ7 c == mkZ7 a <> (mkZ7 b <> mkZ7 c)
        | a <- [0..6], b <- [0..6], c <- [0..6] ]
      `shouldBe` True
