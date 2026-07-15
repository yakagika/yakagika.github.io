-- | fp10.md 「リストモナド」節のコード例
--   (do = for 文, flip map → forM の回収, bind でつなげる依存 2 重ループ).
module Fp10.ListDoSpec (spec) where

import Control.Monad (forM)
import Test.Hspec

pairs :: [(Int, Char)]
pairs = do
  x <- [1, 2, 3]
  c <- "ab"
  pure (x, c)

forLike :: [Int]
forLike = do
  x <- [-3 .. 3 :: Int]
  pure (if x >= 0 then 1 else 0)

halve :: Int -> Maybe Int
halve n = if even n then Just (n `div` 2) else Nothing

trianglePairs :: [(Int, Int)]
trianglePairs = [1 .. 4] >>= \x -> [x .. 4] >>= \y -> pure (x, y)

trianglePairs' :: [(Int, Int)]
trianglePairs' = do
  x <- [1 .. 4]
  y <- [x .. 4]
  pure (x, y)

pythagorean :: [(Int, Int, Int)]
pythagorean = do
  x <- [1 .. 20]
  y <- [x .. 20]
  z <- [y .. 20]
  if x * x + y * y == z * z then pure (x, y, z) else []

spec :: Spec
spec = describe "Fp10.ListDo (リストモナド)" $ do
  it "do の 2 重ループ: 全組み合わせ (直積)" $
    pairs `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b')]
  it "内包表記と同じ計算" $
    pairs `shouldBe` [ (x, c) | x <- [1, 2, 3], c <- "ab" ]
  it "flip map (第6章の for 文風記法) と一致する" $
    forLike `shouldBe` flip map [-3 .. 3] (\x -> if x >= 0 then 1 else 0)
  it "forM: 全部成功なら Just, 一つでも失敗なら Nothing" $ do
    forM [2, 4, 6] halve `shouldBe` Just [1, 2, 3]
    forM [2, 3, 6] halve `shouldBe` Nothing
  it "bind でつなげた依存 2 重ループ (>>= 版と do 版が一致)" $ do
    trianglePairs `shouldBe` trianglePairs'
    trianglePairs `shouldBe` [(1,1),(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,3),(3,4),(4,4)]
  it "空リスト = 候補なし で絞り込み (ピタゴラス数)" $
    pythagorean `shouldBe` [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]
