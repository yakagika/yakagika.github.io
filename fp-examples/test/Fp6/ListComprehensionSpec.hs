-- | fp6.md 「リスト内包表記」節の例と練習問題.
module Fp6.ListComprehensionSpec (spec) where

import Test.Hspec

evens :: [Int]
evens = [x | x <- [1 .. 10], x `mod` 2 == 0]

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1 .. 3], y <- [1 .. 3], x /= y]

-- 練習問題 1: 約数
divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

-- 練習問題 2: ピタゴラス数
pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans n =
  [ (a, b, c)
  | a <- [1 .. n]
  , b <- [a .. n]
  , c <- [b .. n]
  , a * a + b * b == c * c
  ]

spec :: Spec
spec = describe "Fp6.ListComprehension" $ do
  it "evens == [2,4,6,8,10]" $
    evens `shouldBe` [2, 4, 6, 8, 10]
  it "pairs (3x3 で x/=y, 6要素)" $
    pairs `shouldBe`
      [(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)]

  describe "divisors" $ do
    it "divisors 12 == [1,2,3,4,6,12]" $
      divisors 12 `shouldBe` [1, 2, 3, 4, 6, 12]
    it "divisors 13 == [1,13]" $
      divisors 13 `shouldBe` [1, 13]

  describe "pythagoreans" $
    it "pythagoreans 20 (fp6.md の期待値)" $
      pythagoreans 20 `shouldBe`
        [ (3, 4, 5), (5, 12, 13), (6, 8, 10)
        , (8, 15, 17), (9, 12, 15), (12, 16, 20)
        ]
