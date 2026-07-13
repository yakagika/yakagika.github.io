-- | fp9v2.md 「関手を 1 つ手作りする — リストの世界への複写」節のコード例.
--   Bool/Int/String の圏からリストの世界への関手 F (対象 A ↦ [A], 射 f ↦ map f)
--   が恒等射と合成を保つことの機械検査.
module Fp9V2.FunctorConstructSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Fp9V2.FunctorConstruct (手作りの関手 F: A ↦ [A], f ↦ map f)" $ do
  it "射の対応: map fromEnum [False, True] == [0,1]" $
    map fromEnum [False, True] `shouldBe` [0, 1]
  it "恒等射を保つ: map id == id" $
    map id [False, True] `shouldBe` [False, True]
  it "合成を保つ: map (show . fromEnum) == map show . map fromEnum" $
    map (show . fromEnum) [False, True]
      `shouldBe` (map show . map fromEnum) [False, True]
  it "合成した射の値: [\"0\",\"1\"]" $
    map (show . fromEnum) [False, True] `shouldBe` ["0", "1"]
