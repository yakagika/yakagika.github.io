-- | fp12.md 選択課題C「うさぎ個体群シミュレータ」の純粋コア.
--   step (純粋更新) / runSim (State, fp11) がフィボナッチ列になることを検証.
--   IO (mainRabbit) はコンパイル確認のみ.
module Fp12.RabbitSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (total)   -- QuickCheck も total を持つため
import Control.Monad (replicateM)
import Control.Monad.State

-- 個体群: 若いつがい young と 成熟つがい adult (fp7 record)
data Population = Population { young :: Integer, adult :: Integer }
  deriving (Show, Eq)

total :: Population -> Integer
total (Population y a) = y + a

-- 1 か月: 若いつがいは成熟し, 成熟つがいは新たな若いつがいを 1 組産む
step :: Population -> Population
step (Population y a) = Population a (a + y)

-- State: 1 か月進めて, その時点の総数を返す (fp11)
tick :: State Population Integer
tick = do
  modify step
  gets total

simulate :: Int -> State Population [Integer]
simulate n = replicateM n tick

initialPop :: Population
initialPop = Population 1 0   -- 最初は若いつがい 1 組

-- 0 か月目から n か月目までの総数列
runSim :: Int -> [Integer]
runSim n = total initialPop : evalState (simulate n) initialPop

-- IO. コンパイル確認のみ.
mainRabbit :: IO ()
mainRabbit = mapM_ print (runSim 12)

-- 検証: 遷移の公理を property に (fp12 改稿: 代数ファースト).
--   公理 2 本 (young/adult × step) からフィボナッチ再帰が等式変形で従う.
instance Arbitrary Population where
  arbitrary = Population <$> (abs <$> arbitrary) <*> (abs <$> arbitrary)

prop_young_step :: Population -> Bool
prop_young_step p = young (step p) == adult p

prop_adult_step :: Population -> Bool
prop_adult_step p = adult (step p) == total p

prop_fib :: Population -> Bool
prop_fib p = total (step (step p)) == total (step p) + total p

spec :: Spec
spec = describe "Fp12.Rabbit (個体群シミュレータの純粋コア)" $ do
  describe "遷移の公理 (QuickCheck property)" $ do
    prop "young (step p) = adult p (成熟 1 組が若い 1 組を産む)" prop_young_step
    prop "adult (step p) = total p (全員が生き残り成熟する)" prop_adult_step
    prop "帰結: total (step (step p)) = total (step p) + total p (フィボナッチ)" prop_fib
  describe "step (純粋な世代更新)" $ do
    it "step (Population 1 0) == Population 0 1" $
      step (Population 1 0) `shouldBe` Population 0 1
    it "step (Population 1 1) == Population 1 2" $
      step (Population 1 1) `shouldBe` Population 1 2
  describe "total" $
    it "total (Population 5 8) == 13" $
      total (Population 5 8) `shouldBe` 13
  describe "runSim (State, fp11) はフィボナッチ列" $ do
    it "runSim 6 == [1,1,2,3,5,8,13]" $
      runSim 6 `shouldBe` [1, 1, 2, 3, 5, 8, 13]
    it "runSim 0 == [1]" $
      runSim 0 `shouldBe` [1]
