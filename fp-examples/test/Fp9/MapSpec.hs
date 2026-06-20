-- | fp9.md 「Map — キーと値の対応」節のコード例.
module Fp9.MapSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Sum (..))

stock :: Map String Int
stock = Map.fromList [("apple", 3), ("banana", 2)]

wordCount :: [String] -> Map String (Sum Int)
wordCount ws = Map.fromListWith (<>) [(w, Sum 1) | w <- ws]

spec :: Spec
spec = describe "Fp9.Map" $ do
  it "lookup hit: Just 3" $
    Map.lookup "apple" stock `shouldBe` Just 3
  it "lookup miss: Nothing" $
    Map.lookup "grape" stock `shouldBe` Nothing
  it "<> は左偏 union (左の値が残る)" $
    ((Map.fromList [("a", 1), ("b", 2)] <> Map.fromList [("a", 9), ("d", 4)]) :: Map String Int)
      `shouldBe` Map.fromList [("a", 1), ("b", 2), ("d", 4)]
  it "unionWith (<>) は同じキーの値を結合" $
    Map.unionWith (<>) (Map.fromList [("a", Sum 1), ("b", Sum 1)])
                       (Map.fromList [("a", Sum 1), ("c", Sum 1)])
      `shouldBe` Map.fromList [("a", Sum 2), ("b", Sum 1), ("c", Sum 1)]
  it "wordCount で出現回数を集計" $
    wordCount ["a", "b", "a", "a", "b"]
      `shouldBe` Map.fromList [("a", Sum 3), ("b", Sum 2)]
  it "fmap は値に作用 (キーは不変)" $
    fmap (* 10) stock `shouldBe` Map.fromList [("apple", 30), ("banana", 20)]
