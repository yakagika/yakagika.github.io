-- | fp10.md Exercise CH10-5 「interact で行ごとの変換」.
--   純粋コア revLines を検証する (IO 殻 ioMain はコンパイル確認のみ).
module Fp10.Ex105Spec (spec, ioMain) where

import Test.Hspec

revLines :: String -> String
revLines = unlines . map reverse . lines

-- 教材の main 相当 (コンパイル確認のみ)
ioMain :: IO ()
ioMain = interact revLines

spec :: Spec
spec = describe "Fp10.Exercise CH10-5" $ do
  it "revLines \"abc\\ndef\\n\" == \"cba\\nfed\\n\"" $
    revLines "abc\ndef\n" `shouldBe` "cba\nfed\n"
  it "revLines \"\" == \"\" (空入力はそのまま)" $
    revLines "" `shouldBe` ""
  it "1 行でも動く (ghci で IO 抜きに試せる)" $
    revLines "hello\n" `shouldBe` "olleh\n"
