-- | fp9.md 「Maybe — 失敗を型で表す」節のコード例.
module Fp9.MaybeSpec (spec) where

import Test.Hspec

-- 空リストでも停止しない安全な head
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- 0 で割るときは Nothing を返す安全な割り算
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

describeMaybe :: Maybe Int -> String
describeMaybe Nothing  = "値なし"
describeMaybe (Just n) = "値は " ++ show n

spec :: Spec
spec = describe "Fp9.Maybe" $ do
  it "safeHead [1,2,3] == Just 1" $
    safeHead [1, 2, 3 :: Int] `shouldBe` Just 1
  it "safeHead [] == Nothing" $
    safeHead ([] :: [Int]) `shouldBe` Nothing
  it "safeDiv 10 2 == Just 5" $
    safeDiv 10 2 `shouldBe` Just 5
  it "safeDiv 10 0 == Nothing" $
    safeDiv 10 0 `shouldBe` Nothing
  it "describe (safeDiv 10 2) == 値は 5" $
    describeMaybe (safeDiv 10 2) `shouldBe` "値は 5"
  it "describe (safeDiv 10 0) == 値なし" $
    describeMaybe (safeDiv 10 0) `shouldBe` "値なし"
