-- | fp9.md 「Either — 二者択一とエラー表現」節のコード例.
module Fp9.EitherSpec (spec) where

import Test.Hspec

-- 失敗の理由を文字列で返す安全な割り算
safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "0 では割れません"
safeDivE x y = Right (x `div` y)

report :: Either String Int -> String
report (Left err) = "エラー: " ++ err
report (Right n)  = "結果: " ++ show n

spec :: Spec
spec = describe "Fp9.Either" $ do
  it "safeDivE 10 2 == Right 5" $
    safeDivE 10 2 `shouldBe` Right 5
  it "safeDivE 10 0 == Left ..." $
    safeDivE 10 0 `shouldBe` Left "0 では割れません"
  it "report (safeDivE 10 2) == 結果: 5" $
    report (safeDivE 10 2) `shouldBe` "結果: 5"
  it "report (safeDivE 10 0) == エラー: ..." $
    report (safeDivE 10 0) `shouldBe` "エラー: 0 では割れません"
