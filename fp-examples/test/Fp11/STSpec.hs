-- | fp11.md 「ST モナド」節のコード例.
--   runST で可変セルを閉じ込めた sumWithCell が純粋関数として振る舞うことを確認する.
module Fp11.STSpec (spec) where

import Test.Hspec
import Control.Monad.ST
import Data.STRef
import Control.Monad (forM_)

sumWithCell :: [Int] -> Int
sumWithCell xs = runST $ do
  ref <- newSTRef 0
  forM_ xs $ \x ->
    modifySTRef ref (+ x)
  readSTRef ref

spec :: Spec
spec = describe "Fp11.ST (runST に閉じた可変セル)" $ do
  it "sumWithCell [1 .. 100] == 5050 (型は [Int] -> Int で純粋)" $
    sumWithCell [1 .. 100] `shouldBe` 5050
  it "純粋関数なので sum と常に一致する" $
    map sumWithCell [[], [1], [1, 2, 3], [-5, 5]] `shouldBe`
      map sum [[], [1], [1, 2, 3], [-5, 5]]
