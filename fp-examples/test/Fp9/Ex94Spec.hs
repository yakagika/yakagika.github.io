-- | fp9.md Exercise CH9-4 「自作多相型 Box を Functor にする」.
module Fp9.Ex94Spec (spec) where

import Test.Hspec

data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box (f x)

unBox :: Box a -> a
unBox (Box x) = x

spec :: Spec
spec = describe "Fp9.Exercise CH9-4" $ do
  it "unBox (fmap (+1) (Box 10)) == 11" $
    unBox (fmap (+ 1) (Box 10)) `shouldBe` (11 :: Int)
  it "unBox (fmap show (Box 42)) == \"42\"" $
    unBox (fmap show (Box (42 :: Int))) `shouldBe` "42"
  it "関手則: unBox (fmap id (Box 7)) == 7" $
    unBox (fmap id (Box 7)) `shouldBe` (7 :: Int)
