-- | fp11.md 「Reader モナド」節のコード例.
--   自作 Reader のインスタンスと, Config を環境として配る receipt の出力を検証する.
module Fp11.ReaderSpec (spec) where

import Test.Hspec
import Control.Monad (forM)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap g m = Reader $ \r -> g (runReader m r)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  mf <*> mx = Reader $ \r -> (runReader mf r) (runReader mx r)

instance Monad (Reader r) where
  m >>= f = Reader $ \r -> runReader (f (runReader m r)) r

ask :: Reader r r
ask = Reader $ \r -> r

asks :: (r -> a) -> Reader r a
asks f = Reader f

data Config = Config { taxRate :: Double, currencyMark :: String }

priceLabel :: Int -> Reader Config String
priceLabel price = do
  rate <- asks taxRate
  mark <- asks currencyMark
  let total = round (fromIntegral price * (1 + rate)) :: Int
  pure (mark ++ show total)

receipt :: Reader Config [String]
receipt = forM [120, 350, 80] priceLabel

spec :: Spec
spec = describe "Fp11.Reader (環境を配るモナド)" $ do
  describe "部品" $ do
    it "ask は環境そのものを返す" $
      runReader ask (42 :: Int) `shouldBe` 42
    it "asks は環境の一部を読み出す" $
      runReader (asks fst) ((1, 2) :: (Int, Int)) `shouldBe` 1
    it "μ は同じ環境を外側と内側の両方に配る" $ do
      let mm = Reader $ \r -> Reader $ \r' -> (r, r')
      runReader (mm >>= id) (7 :: Int) `shouldBe` (7, 7)

  describe "receipt (税率と通貨記号の設定)" $ do
    it "税率 0.10 のとき" $
      runReader receipt (Config 0.10 "¥") `shouldBe` ["¥132", "¥385", "¥88"]
    it "税率 0.08 のとき (環境 1 つで全体が切り替わる)" $
      runReader receipt (Config 0.08 "¥") `shouldBe` ["¥130", "¥378", "¥86"]
