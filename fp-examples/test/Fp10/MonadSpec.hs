-- | fp10.md 「型クラス Monad」「do 記法」節のコード例.
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

halve :: Int -> Maybe Int
halve n = if even n then Just (n `div` 2) else Nothing

-- do 記法版の calc3 (本文「do 記法」節)
calc3Do :: Int -> Int -> Maybe Int
calc3Do x y = do
  r <- safeDiv x y
  s <- halve r
  pure (r + s)

-- calc3 の >>= 版 (do から機械的に脱糖される形)
calc3Bind :: Int -> Int -> Maybe Int
calc3Bind x y =
  safeDiv x y >>= \r ->
  halve r     >>= \s ->
  pure (r + s)

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
    it "calc3Do (do 版) == calc3Bind (>>= 版) で一致" $
      [calc3Do 100 y | y <- [0, 4, 5, 10]] `shouldBe` [calc3Bind 100 y | y <- [0, 4, 5, 10]]
    it "calc3Do: 3 段連鎖の値 (Just 30 / 途中失敗で Nothing)" $ do
      calc3Do 100 5 `shouldBe` Just 30
      calc3Do 100 4 `shouldBe` Nothing
      calc3Do 100 0 `shouldBe` Nothing
