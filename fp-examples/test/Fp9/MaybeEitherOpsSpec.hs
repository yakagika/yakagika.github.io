-- | fp9.md 「Maybe / Either の基本操作」節のコード例.
module Fp9.MaybeEitherOpsSpec (spec) where

import Test.Hspec
import Data.Maybe (fromMaybe, isJust, catMaybes, mapMaybe)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "0 では割れません"
safeDivE x y = Right (x `div` y)

-- 文字列を Int に変換しようとし, 数字でなければ Nothing
parseInt :: String -> Maybe Int
parseInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

spec :: Spec
spec = describe "Fp9.MaybeEitherOps" $ do
  describe "maybe / fromMaybe / either" $ do
    it "maybe 0 (+100) (Just 5) == 105" $
      maybe 0 (+ 100) (Just 5) `shouldBe` (105 :: Int)
    it "maybe 0 (+100) Nothing == 0" $
      maybe 0 (+ 100) Nothing `shouldBe` (0 :: Int)
    it "fromMaybe (-1) (safeDiv 10 2) == 5" $
      fromMaybe (-1) (safeDiv 10 2) `shouldBe` 5
    it "fromMaybe (-1) (safeDiv 10 0) == -1" $
      fromMaybe (-1) (safeDiv 10 0) `shouldBe` (-1)
    it "either で Right を整形" $
      either ("エラー: " ++) (\n -> "結果: " ++ show n) (safeDivE 10 2)
        `shouldBe` "結果: 5"
    it "either で Left を整形" $
      either ("エラー: " ++) (\n -> "結果: " ++ show n) (safeDivE 10 0)
        `shouldBe` "エラー: 0 では割れません"
  describe "isJust / catMaybes / mapMaybe" $ do
    it "isJust (Just 3) == True" $
      isJust (Just (3 :: Int)) `shouldBe` True
    it "isJust Nothing == False" $
      isJust (Nothing :: Maybe Int) `shouldBe` False
    it "catMaybes [Just 1, Nothing, Just 3] == [1,3]" $
      catMaybes [Just 1, Nothing, Just 3] `shouldBe` [1, 3 :: Int]
    it "mapMaybe parseInt [\"1\",\"x\",\"3\",\"y\"] == [1,3]" $
      mapMaybe parseInt ["1", "x", "3", "y"] `shouldBe` [1, 3]
