-- | fp7.md 「内包表記」節 warn の Data.Set による集合演算.
module Fp7.SetOpsSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

a, b :: Set.Set Int
a = Set.fromList [1, 2, 3, 4]
b = Set.fromList [3, 4, 5, 6]

spec :: Spec
spec = describe "Fp7.SetOps" $ do
  it "union (和集合)" $
    Set.union a b `shouldBe` Set.fromList [1, 2, 3, 4, 5, 6]
  it "intersection (積集合)" $
    Set.intersection a b `shouldBe` Set.fromList [3, 4]
  it "difference (差集合)" $
    Set.difference a b `shouldBe` Set.fromList [1, 2]
  it "member (要素判定)" $
    Set.member 3 a `shouldBe` True
