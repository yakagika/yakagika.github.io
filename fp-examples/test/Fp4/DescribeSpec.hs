-- | fp4.md 「分岐」節の練習問題 `describe`. 4 つの実装すべてを検証.
module Fp4.DescribeSpec (spec) where

import Test.Hspec

-- パターンマッチ版
describePattern :: Int -> String
describePattern 0 = "zero"
describePattern 1 = "one"
describePattern 2 = "two"
describePattern _ = "many"

-- ガード版
describeGuard :: Int -> String
describeGuard n
  | n == 0    = "zero"
  | n == 1    = "one"
  | n == 2    = "two"
  | otherwise = "many"

-- case式版
describeCase :: Int -> String
describeCase n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  _ -> "many"

-- if式版
describeIf :: Int -> String
describeIf n =
  if n == 0 then "zero"
  else if n == 1 then "one"
  else if n == 2 then "two"
  else "many"

spec :: Spec
spec = describe "Fp4.Describe" $ do
  let cases = [ (0, "zero"), (1, "one"), (2, "two")
              , (3, "many"), (10, "many"), (-5, "many")
              ]
  let impls = [ ("pattern", describePattern)
              , ("guard",   describeGuard)
              , ("case",    describeCase)
              , ("if",      describeIf)
              ]
  let runCase (input, expected) (label, impl) =
        it (label ++ ": describe " ++ show input ++ " == " ++ show expected) $
          impl input `shouldBe` expected
  sequence_ [runCase c impl | impl <- impls, c <- cases]
