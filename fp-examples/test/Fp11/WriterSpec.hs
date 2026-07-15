-- | fp11.md 「Writer モナド」節のコード例.
--   自作 Writer (Monoid w が部品) と, コラッツのログ付き計算・モノイド取り替えを検証する.
module Fp11.WriterSpec (spec) where

import Test.Hspec
import Data.Monoid (Sum (..))

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap g (Writer (x, w)) = Writer (g x, w)

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (g, w1) <*> Writer (x, w2) = Writer (g x, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  Writer (x, w1) >>= f =
    let Writer (y, w2) = f x
    in  Writer (y, w1 <> w2)

tell :: w -> Writer w ()
tell w = Writer ((), w)

step :: Int -> Writer [String] Int
step n
  | even n    = do
      tell [show n ++ " は偶数: 2 で割る"]
      pure (n `div` 2)
  | otherwise = do
      tell [show n ++ " は奇数: 3 倍して 1 を足す"]
      pure (3 * n + 1)

collatz :: Int -> Writer [String] Int
collatz 1 = pure 1
collatz n = step n >>= collatz

countStep :: Int -> Writer (Sum Int) Int
countStep n = do
  tell (Sum 1)
  pure (if even n then n `div` 2 else 3 * n + 1)

countCollatz :: Int -> Writer (Sum Int) Int
countCollatz 1 = pure 1
countCollatz n = countStep n >>= countCollatz

spec :: Spec
spec = describe "Fp11.Writer (ログを追記するモナド)" $ do
  describe "モノイドが部品になる" $ do
    it "pure はログなし (mempty) を添える" $
      snd (runWriter (pure 'a' :: Writer [String] Char)) `shouldBe` []
    it ">>= は前後のログを <> で結合する" $
      snd (runWriter (tell ["a"] >> tell ["b"])) `shouldBe` ["a", "b"]

  describe "collatz 6 のログ" $ do
    it "結果は 1" $
      fst (runWriter (collatz 6)) `shouldBe` 1
    it "ログの全行 (本文の実行例と同一)" $
      snd (runWriter (collatz 6)) `shouldBe`
        [ "6 は偶数: 2 で割る"
        , "3 は奇数: 3 倍して 1 を足す"
        , "10 は偶数: 2 で割る"
        , "5 は奇数: 3 倍して 1 を足す"
        , "16 は偶数: 2 で割る"
        , "8 は偶数: 2 で割る"
        , "4 は偶数: 2 で割る"
        , "2 は偶数: 2 で割る"
        ]

  describe "モノイドの取り替え (Sum Int で回数を数える)" $ do
    it "countCollatz 6 は 8 回" $
      getSum (snd (runWriter (countCollatz 6))) `shouldBe` 8
    it "countCollatz 27 は 111 回" $
      getSum (snd (runWriter (countCollatz 27))) `shouldBe` 111
    it "回数はログの行数と一致する (同じ骨組みで集計だけが変わる)" $
      getSum (snd (runWriter (countCollatz 6)))
        `shouldBe` length (snd (runWriter (collatz 6)))
