-- | fp10.md 「組み合わせを手作りする — 積と両立する関手」節のコード例.
--   pairMaybe / pairList = 積をまとめる自然変換 φ : f a × f b → f (a×b) の手作り版.
module Fp10.PairSpec (spec) where

import Test.Hspec

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just x) (Just y) = Just (x, y)
pairMaybe _        _        = Nothing

pairList :: [a] -> [b] -> [(a, b)]
pairList xs ys = [(x, y) | x <- xs, y <- ys]

spec :: Spec
spec = describe "Fp10.Pair (積と両立する関手の手作り)" $ do
  it "pairMaybe (Just 2) (Just 3) == Just (2,3)" $
    pairMaybe (Just 2) (Just (3 :: Int)) `shouldBe` Just (2 :: Int, 3 :: Int)
  it "pairMaybe (Just 2) Nothing == Nothing" $
    pairMaybe (Just (2 :: Int)) (Nothing :: Maybe Int) `shouldBe` Nothing
  it "pairList [1,2] \"ab\" == 直積" $
    pairList [1, 2 :: Int] "ab" `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
  it "組にしてから fmap で足す: Just 5" $
    fmap (\(x, y) -> x + y) (pairMaybe (Just 2) (Just (3 :: Int))) `shouldBe` Just 5
