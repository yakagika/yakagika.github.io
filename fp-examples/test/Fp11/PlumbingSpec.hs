-- | fp11.md 「状態を運ぶ計算」「状態渡しを型にする」節のコード例.
--   手書きの状態配管 (tick / three) と, 手作りの bindS による書き直しが一致することを確認する.
module Fp11.PlumbingSpec (spec) where

import Test.Hspec

tick :: Int -> (Int, Int)
tick n = (n, n + 1)

three :: Int -> ((Int, Int, Int), Int)
three s0 =
  let (a, s1) = tick s0
      (b, s2) = tick s1
      (c, s3) = tick s2
  in  ((a, b, c), s3)

newtype State s a = State { runState :: s -> (a, s) }

bindS :: State s a -> (a -> State s b) -> State s b
bindS m f = State $ \s0 ->
  let (x, s1) = runState m s0
  in  runState (f x) s1

tickS :: State Int Int
tickS = State $ \n -> (n, n + 1)

threeS :: State Int (Int, Int, Int)
threeS =
  tickS `bindS` \a ->
  tickS `bindS` \b ->
  tickS `bindS` \c ->
  State $ \s -> ((a, b, c), s)

spec :: Spec
spec = describe "Fp11.Plumbing (手書きの状態配管と bindS)" $ do
  describe "tick / three (手書きの配管)" $ do
    it "tick 0 == (0,1), tick 5 == (5,6)" $ do
      tick 0 `shouldBe` (0, 1)
      tick 5 `shouldBe` (5, 6)
    it "three 0 == ((0,1,2),3)" $
      three 0 `shouldBe` ((0, 1, 2), 3)

  describe "bindS による書き直し" $ do
    it "runState threeS 0 == ((0,1,2),3) (手書き版と同じ)" $
      runState threeS 0 `shouldBe` ((0, 1, 2), 3)
    it "任意の初期値で three と一致する" $
      map (runState threeS) [0, 1, 10, 100] `shouldBe` map three [0, 1, 10, 100]
