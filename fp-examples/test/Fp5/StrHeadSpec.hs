-- | fp5.md 「パターンマッチ」節の `strHead`.
module Fp5.StrHeadSpec (spec) where

import Test.Hspec

strHead :: Show a => [a] -> String
strHead []    = "Empty"
strHead [x]   = show x
strHead (x:_) = show x

spec :: Spec
spec = describe "Fp5.StrHead" $ do
  it "strHead [] == \"Empty\"" $
    strHead ([] :: [Int]) `shouldBe` "Empty"
  it "strHead [3,4] == \"3\"" $
    strHead [3 :: Int, 4] `shouldBe` "3"
  it "strHead [5] == \"5\"" $
    strHead [5 :: Int] `shouldBe` "5"
