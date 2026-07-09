-- | fp10.md Exercise CH10-4 「2 つの入力を安全に足す — 純粋コアと IO の殻」.
--   純粋コア addInputs を検証する (IO 殻 ioMain はコンパイル確認のみ).
module Fp10.Ex104Spec (spec, ioMain) where

import Test.Hspec
import Text.Read (readMaybe)

addInputs :: String -> String -> Maybe Int
addInputs s t = (+) <$> readMaybe s <*> readMaybe t

-- 教材の main 相当 (コンパイル確認のみ)
ioMain :: IO ()
ioMain = do
  s <- getLine
  t <- getLine
  putStrLn (maybe "数値を入力してください" show (addInputs s t))

spec :: Spec
spec = describe "Fp10.Exercise CH10-4" $ do
  it "addInputs \"3\" \"4\" == Just 7" $
    addInputs "3" "4" `shouldBe` Just 7
  it "addInputs \"3\" \"x\" == Nothing" $
    addInputs "3" "x" `shouldBe` Nothing
  it "addInputs \"x\" \"4\" == Nothing" $
    addInputs "x" "4" `shouldBe` Nothing
  it "maybe による表示文字列 (純粋部分)" $ do
    maybe "数値を入力してください" show (addInputs "3" "4") `shouldBe` "7"
    maybe "数値を入力してください" show (addInputs "3" "x")
      `shouldBe` "数値を入力してください"
