-- | fp8.md 「同値類と商集合 — custom Eq は「商の選択」」節 + Exercise CH8-2 の回答例.
module Fp8.FracSpec (spec) where

import Test.Hspec

data Frac = Frac Int Int deriving Show

instance Eq Frac where
  Frac a b == Frac c d  =  a * d == c * b

fracs :: [Frac]
fracs = [ Frac a b | a <- [-2..2], b <- [1..3] ]

normalize :: Frac -> Frac
normalize (Frac a b) = Frac (a `div` g) (b `div` g)
  where g = gcd (abs a) b

-- 表現 (代表元) そのものを比較するための補助 (== は商の等しさなので)
repr :: Frac -> (Int, Int)
repr (Frac a b) = (a, b)

spec :: Spec
spec = describe "Fp8.Frac (商集合としての有理数, Exercise CH8-2)" $ do
  it "Frac 1 2 == Frac 2 4 (1/2 = 2/4)" $
    (Frac 1 2 == Frac 2 4) `shouldBe` True
  it "Frac 1 2 /= Frac 2 3" $
    (Frac 1 2 == Frac 2 3) `shouldBe` False
  it "Frac 3 6 == Frac 1 2" $
    (Frac 3 6 == Frac 1 2) `shouldBe` True

  describe "手書き == は同値関係 (fracs 上の全数検査)" $ do
    it "反射律" $
      and [ x == x | x <- fracs ] `shouldBe` True
    it "対称律" $
      and [ y == x | x <- fracs, y <- fracs, x == y ] `shouldBe` True
    it "推移律" $
      and [ x == z | x <- fracs, y <- fracs, z <- fracs, x == y, y == z ]
        `shouldBe` True

  describe "normalize (既約分数 = 同値類の代表元)" $ do
    it "normalize (2/4) の表現は (1,2)" $
      repr (normalize (Frac 2 4)) `shouldBe` (1, 2)
    it "normalize (-2/4) の表現は (-1,2)" $
      repr (normalize (Frac (-2) 4)) `shouldBe` (-1, 2)
    it "normalize x == x (正規化しても同じ同値類に留まる)" $
      and [ normalize x == x | x <- fracs ] `shouldBe` True
