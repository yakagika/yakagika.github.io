-- | fp11.md Exercise CH11-4「ユークリッドの互除法にログを付ける」の回答例.
--   仕様: gcd(a, 0) = a, gcd(a, b) = gcd(b, a mod b).
module Fp11.Ex114Spec (spec) where

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

gcdLog :: Int -> Int -> Writer [String] Int
gcdLog a 0 = do
  tell ["gcd " ++ show a ++ " 0 = " ++ show a]
  pure a
gcdLog a b = do
  tell ["gcd " ++ show a ++ " " ++ show b]
  gcdLog b (a `mod` b)

gcdCount :: Int -> Int -> Writer (Sum Int) Int
gcdCount a 0 = pure a
gcdCount a b = do
  tell (Sum 1)
  gcdCount b (a `mod` b)

spec :: Spec
spec = describe "Fp11.Ex114 (Exercise CH11-4: 互除法にログを付ける)" $ do
  it "gcdLog 252 105 の結果は 21" $
    fst (runWriter (gcdLog 252 105)) `shouldBe` 21
  it "gcdLog 252 105 のログ全行" $
    snd (runWriter (gcdLog 252 105)) `shouldBe`
      ["gcd 252 105", "gcd 105 42", "gcd 42 21", "gcd 21 0 = 21"]
  it "gcdCount 252 105: 剰余は 3 回" $
    getSum (snd (runWriter (gcdCount 252 105))) `shouldBe` 3
  it "結果は標準の gcd と一致する" $ do
    let pairs = [(252, 105), (10, 4), (7, 0), (13, 13)]
    [fst (runWriter (gcdLog a b)) | (a, b) <- pairs]
      `shouldBe` [gcd a b | (a, b) <- pairs]
