-- | fp8.md 「モノイド (Monoid)」節のコード例.
module Fp8.MonoidSpec (spec) where

import Test.Hspec

-- 加法のモノイド:  a ⊕ b = a + b,  単位元 e = 0
newtype Add = Add Int deriving (Show, Eq)
(.+.) :: Add -> Add -> Add
Add a .+. Add b = Add (a + b)
instance Semigroup Add where (<>)   = (.+.)
instance Monoid    Add where mempty = Add 0

-- 乗法のモノイド:  a ⊗ b = a × b,  単位元 e = 1
newtype Mul = Mul Int deriving (Show, Eq)
(.*.) :: Mul -> Mul -> Mul
Mul a .*. Mul b = Mul (a * b)
instance Semigroup Mul where (<>)   = (.*.)
instance Monoid    Mul where mempty = Mul 1

spec :: Spec
spec = describe "Fp8.Monoid" $ do
  describe "Add (加法のモノイド)" $ do
    it "Add 3 .+. Add 4 == Add 7"       $ (Add 3 .+. Add 4) `shouldBe` Add 7
    it "Add 3 <> Add 4 == Add 7 (<>=⊕)" $ (Add 3 <> Add 4) `shouldBe` Add 7
    it "Add 3 <> mempty == Add 3"       $ (Add 3 <> mempty) `shouldBe` Add 3
    it "mconcat [Add 1,2,3] == Add 6"   $ mconcat [Add 1, Add 2, Add 3] `shouldBe` Add 6
    it "mempty :: Add == Add 0"         $ (mempty :: Add) `shouldBe` Add 0
  describe "Mul (乗法のモノイド)" $ do
    it "Mul 3 .*. Mul 4 == Mul 12"      $ (Mul 3 .*. Mul 4) `shouldBe` Mul 12
    it "mconcat [Mul 1,2,3] == Mul 6"   $ mconcat [Mul 1, Mul 2, Mul 3] `shouldBe` Mul 6
    it "mempty :: Mul == Mul 1"         $ (mempty :: Mul) `shouldBe` Mul 1
