-- | fp9v2.md 「リストは再帰的な代数的データ型」節のコード例.
module Fp9V2.ListDefSpec (spec) where

import Test.Hspec

-- 組込リスト [a] = [] | a : [a] と同型な自作版 (Nil ↔ [], Cons ↔ (:))
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- 長さ: Nil で 0, Cons で「1 + 残りの長さ」
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- 連結: 左が空なら右をそのまま, Cons なら先頭を残して残りを再帰的に連結
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

spec :: Spec
spec = describe "Fp9V2.ListDef (リストは代数的データ型)" $ do
  it "len (Cons 1 (Cons 2 (Cons 3 Nil))) == 3" $
    len (Cons 1 (Cons 2 (Cons 3 Nil)) :: List Int) `shouldBe` 3
  it "len Nil == 0" $
    len (Nil :: List Int) `shouldBe` 0
  it "append xs ys == [1,2,3,4,5] 相当" $
    append (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 4 (Cons 5 Nil))
      `shouldBe` (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))) :: List Int)
  it "append Nil ys == ys (左単位元)" $
    append Nil (Cons 4 (Cons 5 Nil)) `shouldBe` (Cons 4 (Cons 5 Nil) :: List Int)
