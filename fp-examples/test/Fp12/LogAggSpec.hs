-- | fp12.md 選択課題B「ログ / CSV 集計」の純粋コア.
--   parseHit (Maybe) / Agg モノイド / byStatus (Map) / summarizeLog (mapMaybe + foldMap).
--   IO (mainLog) はコンパイル確認のみ.
module Fp12.LogAggSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map (Map)

-- アクセスログ 1 行 "status,bytes" (CSV)
data Hit = Hit { hitStatus :: Int, hitBytes :: Int }
  deriving (Show, Eq)

-- カンマ区切り (依存追加を避けて自前で分割)
splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitComma rest
  (a, _)          -> [a]

-- 壊れた行は Nothing で捨てる (fp9 Maybe + Applicative)
parseHit :: String -> Maybe Hit
parseHit line = case splitComma line of
  [s, b] -> Hit <$> readMaybe s <*> readMaybe b
  _      -> Nothing

-- 件数と総バイトを集計するモノイド (fp8)
data Agg = Agg { aCount :: Int, aBytes :: Int }
  deriving (Show, Eq)

instance Semigroup Agg where
  Agg c1 b1 <> Agg c2 b2 = Agg (c1 + c2) (b1 + b2)

instance Monoid Agg where
  mempty = Agg 0 0

aggregate :: [Hit] -> Agg
aggregate = foldMap (\h -> Agg 1 (hitBytes h))

-- ステータス別件数 (fp9 Map)
byStatus :: [Hit] -> Map Int Int
byStatus = Map.fromListWith (+) . map (\h -> (hitStatus h, 1))

-- パイプライン: 壊れた行を mapMaybe で除外して集計 (fp9 + fp8)
summarizeLog :: [String] -> (Agg, Map Int Int)
summarizeLog ls = let hits = mapMaybe parseHit ls
                  in (aggregate hits, byStatus hits)

-- IO (標準入力を集計). コンパイル確認のみ.
mainLog :: IO ()
mainLog = do
  contents <- getContents
  let (agg, codes) = summarizeLog (lines contents)
  putStrLn ("件数 " ++ show (aCount agg) ++ " / 総バイト " ++ show (aBytes agg))
  mapM_ (\(c, n) -> putStrLn (show c ++ ": " ++ show n)) (Map.toList codes)

-- 検証: 往復と準同型を公理として property に (fp12 改稿: 代数ファースト)
instance Arbitrary Hit where
  arbitrary = Hit <$> arbitrary <*> arbitrary

prop_parse_roundtrip :: Int -> Int -> Bool
prop_parse_roundtrip s b = parseHit (show s ++ "," ++ show b) == Just (Hit s b)

prop_agg_hom :: [Hit] -> [Hit] -> Bool
prop_agg_hom xs ys = aggregate (xs ++ ys) == aggregate xs <> aggregate ys

prop_byStatus_hom :: [Hit] -> [Hit] -> Bool
prop_byStatus_hom xs ys =
  byStatus (xs ++ ys) == Map.unionWith (+) (byStatus xs) (byStatus ys)

spec :: Spec
spec = describe "Fp12.LogAgg (ログ集計の純粋コア)" $ do
  describe "公理 (QuickCheck property)" $ do
    prop "往復: parseHit (show s ++ \",\" ++ show b) = Just (Hit s b)" prop_parse_roundtrip
    prop "aggregate は準同型: aggregate (xs++ys) = aggregate xs <> aggregate ys" prop_agg_hom
    prop "byStatus は準同型: byStatus (xs++ys) = unionWith (+) ..." prop_byStatus_hom
  describe "parseHit (Maybe, 壊れた行は捨てる)" $ do
    it "正常" $ parseHit "200,1024" `shouldBe` Just (Hit 200 1024)
    it "数値でない" $ parseHit "200,abc" `shouldBe` Nothing
    it "フィールド数不正" $ parseHit "200" `shouldBe` Nothing
  describe "aggregate (Monoid foldMap)" $ do
    it "件数と総バイト" $
      aggregate [Hit 200 1000, Hit 404 50, Hit 200 2000] `shouldBe` Agg 3 3050
    it "空は mempty" $ aggregate [] `shouldBe` (mempty :: Agg)
  describe "byStatus (Map, fp9)" $
    it "ステータス別件数" $
      byStatus [Hit 200 1, Hit 200 2, Hit 404 3]
        `shouldBe` Map.fromList [(200, 2), (404, 1)]
  describe "summarizeLog (mapMaybe で壊れた行を除外)" $
    it "壊れた行を飛ばして集計" $
      fst (summarizeLog ["200,1000", "broken", "404,50", "200,2000"])
        `shouldBe` Agg 3 3050
  describe "準同型 (fp8): aggregate (xs ++ ys) == aggregate xs <> aggregate ys" $
    it "分割して集計しても同じ" $
      let xs = [Hit 200 1000, Hit 404 50]
          ys = [Hit 200 2000]
      in aggregate (xs ++ ys) `shouldBe` aggregate xs <> aggregate ys
