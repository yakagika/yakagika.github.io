-- | fp5.md 「変数」節の let / where 例.
module Fp5.LocalVariablesSpec (spec) where

import Test.Hspec

-- let 1個
someFuncLet1 :: Int -> Int
someFuncLet1 y =
  let x = 1
  in  x + y

-- let 複数
someFuncLet2 :: Int -> Int
someFuncLet2 z =
  let x = 1
      y = 2
  in  x + y + z

-- where
someFuncWhere :: Int -> Int
someFuncWhere z = func z
  where
    x = 1
    y = 2
    func w = x + y + w

-- トップレベル変数
xTop :: Int
xTop = 1

someFuncWithTop :: Int -> Int
someFuncWithTop y = xTop + y

spec :: Spec
spec = describe "Fp5.LocalVariables" $ do
  it "let の単一束縛 someFuncLet1 1 == 2" $
    someFuncLet1 1 `shouldBe` 2
  it "let の複数束縛 someFuncLet2 1 == 4" $
    someFuncLet2 1 `shouldBe` 4
  it "where someFuncWhere 1 == 4" $
    someFuncWhere 1 `shouldBe` 4
  it "トップレベル someFuncWithTop 1 == 2" $
    someFuncWithTop 1 `shouldBe` 2
