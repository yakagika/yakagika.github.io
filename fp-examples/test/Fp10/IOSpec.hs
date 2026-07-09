-- | fp10.md 「入出力 (IO)」各節のコード例.
--   IO アクション (教材の main 相当) はコンパイル確認のみで hspec の検証対象外
--   (fp12 CalcSpec の runRepl と同方針). 純粋コア (greet 等) を検証する.
module Fp10.IOSpec
  ( spec
  , greetAction
  , ioGreetTwice
  , ioAskName
  , ioAskName'
  , ioUpper
  , ioLetExample
  ) where

import Test.Hspec
import Data.Char (toUpper)

-- 純粋コア: ロジックはただの関数
greet :: String -> String
greet name = "こんにちは, " ++ name ++ " さん!"

greetAction :: IO ()
greetAction = putStrLn "こんにちは"   -- 定義しただけでは何も表示されない

-- 教材の main 相当 (2 回組み込めば 2 回実行される)
ioGreetTwice :: IO ()
ioGreetTwice = do
  greetAction
  greetAction

-- 教材の main 相当 (名前を尋ねて挨拶)
ioAskName :: IO ()
ioAskName = do
  putStrLn "お名前は?"
  name <- getLine          -- アクションの実行結果を name に束縛
  putStrLn (greet name)    -- 純粋関数は IO の中で自由に使える

-- do を >>= / >> に脱糖した形. do 版とまったく同じプログラム.
ioAskName' :: IO ()
ioAskName' =
  putStrLn "お名前は?" >>
  getLine >>= \name ->
  putStrLn (greet name)

-- interact: 入力をすべて大文字にして出力する
ioUpper :: IO ()
ioUpper = interact (map toUpper)

-- <- (アクションの結果) と let (純粋な計算) の区別
ioLetExample :: IO ()
ioLetExample = do
  name <- getLine
  let msg = greet name
  putStrLn msg

spec :: Spec
spec = describe "Fp10.IO (純粋コア)" $ do
  it "greet \"太郎\" == こんにちは, 太郎 さん!" $
    greet "太郎" `shouldBe` "こんにちは, 太郎 さん!"
  it "print の純粋部分: show 3 == \"3\" (print = putStrLn . show)" $
    show (3 :: Int) `shouldBe` "3"
  it "大文字化の純粋コア: map toUpper" $
    map toUpper "abc" `shouldBe` "ABC"
