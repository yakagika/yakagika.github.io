-- | fp5.md 「集合と列挙型」節の練習問題.
module Fp5.WeekdaySpec (spec) where

import Test.Hspec

data Weekday = Sunday | Monday | Tuesday | Wednesday
             | Thursday | Friday | Saturday
             deriving (Show, Eq)

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

nextDay :: Weekday -> Weekday
nextDay Sunday    = Monday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday

spec :: Spec
spec = describe "Fp5.Weekday" $ do
  describe "isWeekend" $ do
    it "Sunday is weekend"    $ isWeekend Sunday `shouldBe` True
    it "Saturday is weekend"  $ isWeekend Saturday `shouldBe` True
    it "Monday is not"        $ isWeekend Monday `shouldBe` False
    it "Wednesday is not"     $ isWeekend Wednesday `shouldBe` False

  describe "nextDay (循環)" $ do
    it "Friday → Saturday"   $ nextDay Friday `shouldBe` Saturday
    it "Saturday → Sunday"   $ nextDay Saturday `shouldBe` Sunday
    it "Sunday → Monday"     $ nextDay Sunday `shouldBe` Monday
    it "週を循環すれば元に戻る"  $ iterate nextDay Sunday !! 7 `shouldBe` Sunday
