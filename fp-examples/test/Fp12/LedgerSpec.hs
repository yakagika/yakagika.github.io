-- | fp12.md 選択課題A「家計簿 / 取引台帳 CLI」の純粋コア.
--   parseTx (Either) / Summary モノイド / byCategory (Map) / runLedger (State).
--   IO バッチ (mainLedger) はコンパイル確認のみ.
module Fp12.LedgerSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Read (readMaybe)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

-- カテゴリ (列挙型, fp7/fp8)
data Category = Food | Transport | Utility | Income | Other
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 取引 (金額は 収入 +, 支出 -)
data Tx = Tx { txCat :: Category, txAmount :: Int }
  deriving (Show, Eq)

data ParseError = UnknownCategory String | BadAmount String | BadFormat String
  deriving (Show, Eq)

-- "food -500" のような 1 行を取引にパース (fp9 Either / fp10 do)
parseTx :: String -> Either ParseError Tx
parseTx line = case words line of
  [c, a] -> do
    cat <- parseCat c
    amt <- parseAmt a
    Right (Tx cat amt)
  _ -> Left (BadFormat line)
  where
    parseCat "food"      = Right Food
    parseCat "transport" = Right Transport
    parseCat "utility"   = Right Utility
    parseCat "income"    = Right Income
    parseCat "other"     = Right Other
    parseCat s           = Left (UnknownCategory s)
    parseAmt s = maybe (Left (BadAmount s)) Right (readMaybe s)

-- 件数と残高を集計するモノイド (fp8 章末 Stats と同型)
data Summary = Summary { sCount :: Int, sBalance :: Int }
  deriving (Show, Eq)

instance Semigroup Summary where
  Summary c1 b1 <> Summary c2 b2 = Summary (c1 + c2) (b1 + b2)

instance Monoid Summary where
  mempty = Summary 0 0

summarize :: [Tx] -> Summary
summarize = foldMap (\t -> Summary 1 (txAmount t))

-- カテゴリ別の合計 (fp9 Map)
byCategory :: [Tx] -> Map Category Int
byCategory = Map.fromListWith (+) . map (\t -> (txCat t, txAmount t))

-- 1 行処理: パースできれば台帳に積み, メッセージを返す (fp11 State)
processLine :: String -> State [Tx] String
processLine line = case parseTx line of
  Right tx -> do
    modify (tx :)
    n <- gets length
    return ("記録 " ++ show tx ++ " / 件数 " ++ show n)
  Left err -> return ("無視 (" ++ show err ++ ")")

runLedger :: [String] -> ([String], Summary)
runLedger ls =
  let (msgs, ledger) = runState (mapM processLine ls) []
  in (msgs, summarize ledger)

-- IO バッチ: 標準入力を全部読んで集計 (コンパイル確認のみ)
mainLedger :: IO ()
mainLedger = do
  contents <- getContents
  let (msgs, summ) = runLedger (lines contents)
  mapM_ putStrLn msgs
  putStrLn ("合計残高 " ++ show (sBalance summ) ++ " / 件数 " ++ show (sCount summ))

-- 検証: 集計の準同型を公理として property に (fp12 改稿: 代数ファースト)
instance Arbitrary Category where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Tx where
  arbitrary = Tx <$> arbitrary <*> arbitrary

prop_summarize_hom :: [Tx] -> [Tx] -> Bool
prop_summarize_hom xs ys = summarize (xs ++ ys) == summarize xs <> summarize ys

prop_byCategory_hom :: [Tx] -> [Tx] -> Bool
prop_byCategory_hom xs ys =
  byCategory (xs ++ ys) == Map.unionWith (+) (byCategory xs) (byCategory ys)

spec :: Spec
spec = describe "Fp12.Ledger (家計簿 CLI の純粋コア)" $ do
  describe "集計の公理 (QuickCheck property)" $ do
    prop "summarize は準同型: summarize (xs++ys) = summarize xs <> summarize ys" prop_summarize_hom
    prop "byCategory は準同型: byCategory (xs++ys) = unionWith (+) ..." prop_byCategory_hom
  describe "parseTx (Either)" $ do
    it "正常行" $ parseTx "food -500" `shouldBe` Right (Tx Food (-500))
    it "収入" $ parseTx "income 2000" `shouldBe` Right (Tx Income 2000)
    it "未知カテゴリ" $ parseTx "candy -100" `shouldBe` Left (UnknownCategory "candy")
    it "金額不正" $ parseTx "food xxx" `shouldBe` Left (BadAmount "xxx")
    it "書式不正 (語数違い)" $ parseTx "food" `shouldBe` Left (BadFormat "food")
  describe "Monoid 集計" $ do
    it "summarize" $
      summarize [Tx Food (-500), Tx Income 2000] `shouldBe` Summary 2 1500
    it "空は mempty" $ summarize [] `shouldBe` (mempty :: Summary)
  describe "byCategory (Map, fp9)" $
    it "カテゴリ別合計" $
      byCategory [Tx Food (-300), Tx Food (-200), Tx Income 1000]
        `shouldBe` Map.fromList [(Food, -500), (Income, 1000)]
  describe "runLedger (State, fp11)" $
    it "複数行を処理し, 壊れた行は無視して集計" $
      snd (runLedger ["food -500", "income 2000", "bad line here"])
        `shouldBe` Summary 2 1500
