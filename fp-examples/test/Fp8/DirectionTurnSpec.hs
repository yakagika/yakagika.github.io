-- | fp8.md 演習 CH8-5 の回答例.
module Fp8.DirectionTurnSpec (spec) where

import Test.Hspec

data Direction = North | East | South | West
  deriving (Show, Eq, Enum, Bounded)

turnRight :: Direction -> Direction
turnRight d
  | d == maxBound = minBound
  | otherwise     = succ d

spec :: Spec
spec = describe "Fp8.DirectionTurn" $ do
  it "turnRight North == East"  $ turnRight North `shouldBe` East
  it "turnRight East == South"  $ turnRight East  `shouldBe` South
  it "turnRight South == West"  $ turnRight South `shouldBe` West
  it "turnRight West == North"  $ turnRight West  `shouldBe` North
  it "[minBound..maxBound :: Direction] == [North,East,South,West]" $
    [minBound .. maxBound :: Direction] `shouldBe` [North, East, South, West]
