-- | fp10.md 「Monad — 連鎖の型クラス化 (>>= と return)」「do 記法 — >>= の脱糖」節のコード例.
--   >>= / Kleisli 合成 >=> / do 記法 が同一のものであることを確認する.
module Fp10.MonadSpec (spec) where

import Test.Hspec
import Control.Monad ((>=>))

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- >>= 版
calc :: Int -> Maybe Int
calc x = safeDiv 100 x >>= \r -> safeDiv r 2

-- Kleisli 合成 >=> 版 (calc と同じ)
calc' :: Int -> Maybe Int
calc' = safeDiv 100 >=> \r -> safeDiv r 2

-- do 記法版 (calc と同じ / 機械的に脱糖される)
calcDo :: Int -> Maybe Int
calcDo x = do
  r <- safeDiv 100 x
  safeDiv r 2

spec :: Spec
spec = describe "Fp10.Monad" $ do
  describe "Maybe モナドの >>= (失敗の連鎖)" $ do
    it "calc 5 == Just 10 (100/5=20, 20/2=10)" $
      calc 5 `shouldBe` Just 10
    it "calc 0 == Nothing (最初の割り算で失敗 → 連鎖が止まる)" $
      calc 0 `shouldBe` Nothing
    it "safeDiv 100 x が Nothing なら後続は呼ばれない" $
      (safeDiv 100 0 >>= \r -> safeDiv r 2) `shouldBe` Nothing

  describe ">>= / >=> / do は同じ計算" $ do
    it "calc == calc' (>=> 版) で一致" $
      map calc [1, 2, 5, 0, 4] `shouldBe` map calc' [1, 2, 5, 0, 4]
    it "calc == calcDo (do 版) で一致" $
      map calc [1, 2, 5, 0, 4] `shouldBe` map calcDo [1, 2, 5, 0, 4]
