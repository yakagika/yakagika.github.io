-- | fp10.md 「モナド則」節. Maybe モナドで 3 つのモナド則を QuickCheck で確認する.
--   コンパイラは則を検査しないので, テストで (証明ではなく) 経験的に確かめる.
module Fp10.MonadLawSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- 則の確認に使う 2 つの Kleisli 射
f :: Int -> Maybe Int
f x = if even x then Just (x + 1) else Nothing

g :: Int -> Maybe Int
g x = Just (x * 2)

spec :: Spec
spec = describe "Fp10.MonadLaw (Maybe)" $ do
  prop "左単位元: return a >>= f == f a" $ \a ->
    (return a >>= f) == f (a :: Int)
  prop "右単位元: m >>= return == m" $ \m ->
    (m >>= return) == (m :: Maybe Int)
  prop "結合律: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $ \m ->
    ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g) :: Maybe Int)
