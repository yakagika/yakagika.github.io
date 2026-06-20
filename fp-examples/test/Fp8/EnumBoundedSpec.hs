-- | fp8.md 「Enum と Bounded — 列挙」節のコード例.
module Fp8.EnumBoundedSpec (spec) where

import Test.Hspec

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

nextDay :: Weekday -> Weekday
nextDay d
  | d == maxBound = minBound
  | otherwise     = succ d

allDays :: [Weekday]
allDays = [minBound .. maxBound]

spec :: Spec
spec = describe "Fp8.EnumBounded" $ do
  it "nextDay Friday == Saturday"         $ nextDay Friday              `shouldBe` Saturday
  it "nextDay Saturday == Sunday"         $ nextDay Saturday            `shouldBe` Sunday
  it "length allDays == 7"               $ length allDays              `shouldBe` 7
  it "head allDays == Sunday"            $ head allDays                `shouldBe` Sunday
  it "last allDays == Saturday"          $ last allDays                `shouldBe` Saturday
  it "iterate nextDay Sunday !! 7 == Sunday" $ (iterate nextDay Sunday !! 7) `shouldBe` Sunday
