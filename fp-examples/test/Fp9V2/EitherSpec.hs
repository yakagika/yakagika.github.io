-- | fp9v2.md 「Either — 二者択一とエラー表現」節のコード例.
--   専用エラー型 DivError を主, 文字列版 safeDivS を副例として検証する.
module Fp9V2.EitherSpec (spec) where

import Test.Hspec

-- 起こりうる失敗を列挙した専用のエラー型 (いまは 1 種類)
data DivError = DivByZero
  deriving (Show, Eq)

safeDivE :: Int -> Int -> Either DivError Int
safeDivE _ 0 = Left DivByZero
safeDivE x y = Right (x `div` y)

-- エラー値を人間向けメッセージへ (表示を値から分離する)
renderDivError :: DivError -> String
renderDivError DivByZero = "0 では割れません"

report :: Either DivError Int -> String
report (Left e)  = "エラー: " ++ renderDivError e
report (Right n) = "結果: " ++ show n

-- 文字列版 (note の副例): 手軽だが閉じた集合でも網羅検査でもない
safeDivS :: Int -> Int -> Either String Int
safeDivS _ 0 = Left "0 では割れません"
safeDivS x y = Right (x `div` y)

spec :: Spec
spec = describe "Fp9V2.Either" $ do
  it "safeDivE 10 2 == Right 5" $
    safeDivE 10 2 `shouldBe` Right 5
  it "safeDivE 10 0 == Left DivByZero" $
    safeDivE 10 0 `shouldBe` Left DivByZero
  it "report (safeDivE 10 2) == 結果: 5" $
    report (safeDivE 10 2) `shouldBe` "結果: 5"
  it "report (safeDivE 10 0) == エラー: 0 では割れません" $
    report (safeDivE 10 0) `shouldBe` "エラー: 0 では割れません"
  it "renderDivError DivByZero == 0 では割れません" $
    renderDivError DivByZero `shouldBe` "0 では割れません"
  it "safeDivS (文字列版) も同じ結果を返す" $
    safeDivS 10 0 `shouldBe` Left "0 では割れません"
