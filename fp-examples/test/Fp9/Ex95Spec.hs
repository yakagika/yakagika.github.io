-- | fp9.md Exercise CH9-5 「自然変換 firstTwo の実装」.
module Fp9.Ex95Spec (spec) where

import Test.Hspec

firstTwo :: [a] -> Maybe (a, a)
firstTwo (x : y : _) = Just (x, y)
firstTwo _           = Nothing

spec :: Spec
spec = describe "Fp9.Exercise CH9-5" $ do
  it "firstTwo [1,2,3] == Just (1,2)" $
    firstTwo [1, 2, 3 :: Int] `shouldBe` Just (1, 2)
  it "firstTwo [1] == Nothing" $
    firstTwo [1 :: Int] `shouldBe` Nothing
  it "firstTwo [] == Nothing" $
    firstTwo ([] :: [Int]) `shouldBe` Nothing
  it "firstTwo \"abc\" == Just ('a','b')" $
    firstTwo "abc" `shouldBe` Just ('a', 'b')
