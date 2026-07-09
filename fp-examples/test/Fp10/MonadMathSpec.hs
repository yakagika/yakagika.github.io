-- | fp10.md 「モナドの正体 — 自己関手と単位・乗法 (η, μ)」節のコード例.
--   η = return, μ = join. >>= = join . fmap, および (η,μ) 形のモナド則を確認する.
module Fp10.MonadMathSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad (join)

-- 則の確認に使う Kleisli 射
f :: Int -> Maybe Int
f x = if even x then Just (x + 1) else Nothing

spec :: Spec
spec = describe "Fp10.MonadMath (η=return / μ=join)" $ do
  describe "join = μ : m (m a) -> m a (入れ子をひと重に潰す)" $ do
    it "join (Just (Just 3)) == Just 3" $
      join (Just (Just 3)) `shouldBe` Just (3 :: Int)
    it "join (Just Nothing) == Nothing" $
      join (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "join Nothing == Nothing" $
      join (Nothing :: Maybe (Maybe Int)) `shouldBe` Nothing
    it "join [[1,2],[3]] == [1,2,3] (リストの join は concat)" $
      join [[1, 2], [3]] `shouldBe` [1, 2, 3 :: Int]

  describe ">>= と join / fmap の関係" $ do
    it "m >>= f == join (fmap f m)" $
      (Just 4 >>= f) `shouldBe` join (fmap f (Just (4 :: Int)))
    it "Nothing でも m >>= f == join (fmap f m)" $
      (Nothing >>= f) `shouldBe` join (fmap f (Nothing :: Maybe Int))
    it "join m == m >>= id" $
      join (Just (Just 5)) `shouldBe` (Just (Just 5) >>= id :: Maybe Int)

  describe "モナド則の (η, μ) 形 (Maybe, QuickCheck)" $ do
    prop "結合律: join . fmap join == join . join" $ \mmm ->
      join (fmap join mmm) == join (join (mmm :: Maybe (Maybe (Maybe Int))))
    prop "単位律: join . fmap return == id" $ \m ->
      join (fmap return m) == (m :: Maybe Int)
    prop "単位律: join . return == id" $ \m ->
      join (return m) == (m :: Maybe Int)
