-- | fp8.md 「ブール代数」節のコード例.
module Fp8.BoolAlgebraSpec (spec) where

import Test.Hspec

data Bool2 = T | F deriving (Show, Eq)

-- 論理積 ∧ に対応する演算子 (.&.)
(.&.) :: Bool2 -> Bool2 -> Bool2
T .&. T = T
_ .&. _ = F

-- 論理和 ∨ に対応する演算子 (.|.)
(.|.) :: Bool2 -> Bool2 -> Bool2
F .|. F = F
_ .|. _ = T

-- 否定 ¬ は 1 項演算なので関数のまま
not2 :: Bool2 -> Bool2
not2 T = F
not2 F = T

-- 論理積のモノイド: 全部 T なら T  (<> = ∧)
newtype All2 = All2 { getAll2 :: Bool2 } deriving (Show, Eq)
instance Semigroup All2 where All2 a <> All2 b = All2 (a .&. b)
instance Monoid    All2 where mempty = All2 T

-- 論理和のモノイド: 1 つでも T なら T  (<> = ∨)
newtype Any2 = Any2 { getAny2 :: Bool2 } deriving (Show, Eq)
instance Semigroup Any2 where Any2 a <> Any2 b = Any2 (a .|. b)
instance Monoid    Any2 where mempty = Any2 F

spec :: Spec
spec = describe "Fp8.BoolAlgebra" $ do
  describe "Bool2 の演算" $ do
    it "T .&. F == F" $ (T .&. F) `shouldBe` F
    it "T .&. T == T" $ (T .&. T) `shouldBe` T
    it "T .|. F == T" $ (T .|. F) `shouldBe` T
    it "F .|. F == F" $ (F .|. F) `shouldBe` F
    it "not2 T == F"  $ not2 T `shouldBe` F
    it "not2 F == T"  $ not2 F `shouldBe` T
  describe "補元律" $ do
    it "x ∧ ¬x == F" $ (T .&. not2 T) `shouldBe` F
    it "x ∨ ¬x == T" $ (T .|. not2 T) `shouldBe` T
  describe "All2 (論理積のモノイド)" $ do
    it "getAll2 (All2 T <> All2 F) == F" $
      getAll2 (All2 T <> All2 F) `shouldBe` F
    it "getAll2 (mconcat [All2 T, All2 T]) == T" $
      getAll2 (mconcat [All2 T, All2 T]) `shouldBe` T
    it "getAll2 (mempty :: All2) == T" $
      getAll2 (mempty :: All2) `shouldBe` T
  describe "Any2 (論理和のモノイド)" $ do
    it "getAny2 (Any2 F <> Any2 T) == T" $
      getAny2 (Any2 F <> Any2 T) `shouldBe` T
    it "getAny2 (mempty :: Any2) == F" $
      getAny2 (mempty :: Any2) `shouldBe` F
    it "getAny2 (mconcat [Any2 F, Any2 F]) == F" $
      getAny2 (mconcat [Any2 F, Any2 F]) `shouldBe` F
