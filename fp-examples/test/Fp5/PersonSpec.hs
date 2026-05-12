-- | fp5.md 「直積型 レコード構文」節の練習問題 `Person`.
module Fp5.PersonSpec (spec) where

import Test.Hspec

data Person = Person
  { personName  :: String
  , personAge   :: Int
  , personEmail :: String
  } deriving (Show, Eq)

birthday :: Person -> Person
birthday p = p { personAge = personAge p + 1 }

totalAge :: Person -> Person -> Int
totalAge p1 p2 = personAge p1 + personAge p2

spec :: Spec
spec = describe "Fp5.Person" $ do
  let alice = Person { personName = "Alice"
                     , personAge = 30
                     , personEmail = "alice@example.com"
                     }
      bob   = Person { personName = "Bob"
                     , personAge = 25
                     , personEmail = "bob@example.com"
                     }

  it "birthday alice の年齢が 31 に増える" $
    personAge (birthday alice) `shouldBe` 31
  it "birthday は name / email を維持する" $ do
    personName (birthday alice) `shouldBe` "Alice"
    personEmail (birthday alice) `shouldBe` "alice@example.com"
  it "totalAge alice bob == 55" $
    totalAge alice bob `shouldBe` 55
