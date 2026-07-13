-- | fp7.md Exercise CH7-6 (同値関係の判定と全数検査) の回答例.
module Fp7.SameLengthSpec (spec) where

import Test.Hspec

sameLength :: String -> String -> Bool
sameLength x y = length x == length y

evenProd :: Int -> Int -> Bool
evenProd x y = even (x * y)

words0 :: [String]
words0 = ["a", "ab", "abc", "xy", "z"]

spec :: Spec
spec = describe "Fp7.SameLength (Exercise CH7-6)" $ do
  describe "sameLength" $ do
    it "sameLength \"ab\" \"xy\" == True"   $ sameLength "ab" "xy" `shouldBe` True
    it "sameLength \"ab\" \"abc\" == False" $ sameLength "ab" "abc" `shouldBe` False

  describe "sameLength は words0 上の同値関係" $ do
    it "反射律" $
      and [ sameLength x x | x <- words0 ] `shouldBe` True
    it "対称律" $
      and [ sameLength y x | x <- words0, y <- words0
          , sameLength x y ] `shouldBe` True
    it "推移律" $
      and [ sameLength x z | x <- words0, y <- words0, z <- words0
          , sameLength x y, sameLength y z ] `shouldBe` True

  describe "evenProd は同値関係ではない" $ do
    it "反射律が破れる (1..10 の全数検査が False)" $
      and [ evenProd x x | x <- [1..10] ] `shouldBe` False
    it "反例: evenProd 1 1 == False" $
      evenProd 1 1 `shouldBe` False
    it "対称律は満たす (1..10)" $
      and [ evenProd y x | x <- [1..10], y <- [1..10], evenProd x y ]
        `shouldBe` True
    it "推移律も破れる: 1R2, 2R3 だが 1R3 でない" $ do
      (evenProd 1 2 && evenProd 2 3) `shouldBe` True
      evenProd 1 3 `shouldBe` False
