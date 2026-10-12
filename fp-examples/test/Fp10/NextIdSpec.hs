module Fp10.NextIdSpec (spec) where

import Test.Hspec

nextId :: Int -> Int
nextId n = n + 1

spec :: Spec
spec = describe "Fp10.NextId (参照透過性)" $ do
  it "nextId 1 == 2" $ nextId 1 `shouldBe` 2
  it "let で束縛しても, 式を並べても, 値で置き換えても同じ" $ do
    (let y = nextId 1 in y + y) `shouldBe` nextId 1 + nextId 1
    nextId 1 + nextId 1 `shouldBe` 2 + 2
