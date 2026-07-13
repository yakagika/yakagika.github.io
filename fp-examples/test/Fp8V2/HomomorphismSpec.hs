-- | fp8v2.md 「準同型 — 構造を保つ写像」章 + Exercise CH8-6 の回答例.
module Fp8V2.HomomorphismSpec (spec) where

import Test.Hspec

data Frac = Frac Int Int deriving Show

instance Eq Frac where
  Frac a b == Frac c d  =  a * d == c * b

fracs :: [Frac]
fracs = [ Frac a b | a <- [-2..2], b <- [1..3] ]

double :: Frac -> Frac
double (Frac a b) = Frac (2 * a) b

num :: Frac -> Int
num (Frac a _) = a

xss :: [String]
xss = ["", "a", "ab", "abc"]

spec :: Spec
spec = describe "Fp8V2.Homomorphism (Exercise CH8-6)" $ do
  describe "length はモノイド準同型 ([a],++,[]) → (Int,+,0)" $ do
    it "演算を保つ: length (xs ++ ys) == length xs + length ys" $
      and [ length (xs ++ ys) == length xs + length ys
          | xs <- xss, ys <- xss ] `shouldBe` True
    it "単位元を保つ: length [] == 0" $
      length ([] :: String) `shouldBe` 0

  describe "double は同値を保つ (well-defined)" $
    it "x == y ならば double x == double y (fracs 全対)" $
      and [ double x == double y | x <- fracs, y <- fracs, x == y ]
        `shouldBe` True

  describe "num (分子を返す) は同値を保たない" $ do
    it "Frac 1 2 == Frac 2 4 (同値)" $
      (Frac 1 2 == Frac 2 4) `shouldBe` True
    it "だが num の値は 1 /= 2" $
      (num (Frac 1 2) == num (Frac 2 4)) `shouldBe` False

  describe "商への射影は演算を保つ (mod 7)" $
    it "(a+b) mod 7 == ((a mod 7) + (b mod 7)) mod 7 (0..20 全対)" $
      and [ (a + b) `mod` 7 == ((a `mod` 7) + (b `mod` 7)) `mod` 7
          | a <- [0..20 :: Int], b <- [0..20] ] `shouldBe` True
