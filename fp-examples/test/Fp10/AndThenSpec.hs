-- | fp10.md 「結果に応じた計算」節のコード例.
--   bind = Maybe 専用の >>= の手作り版 (join . fmap の Maybe 形).
module Fp10.AndThenSpec (spec) where

import Test.Hspec

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just x) f = f x

calc :: Int -> Maybe Int
calc x = safeDiv 100 x `bind` \r -> safeDiv r 2

halve :: Int -> Maybe Int
halve n = if even n then Just (n `div` 2) else Nothing

assertNonNeg :: Int -> Maybe Int
assertNonNeg n = if n >= 0 then Just n else Nothing

calc3 :: Int -> Int -> Maybe Int
calc3 x y =
  safeDiv x y `bind` \r ->
  halve r     `bind` \s ->
  Just (r + s)

spec :: Spec
spec = describe "Fp10.AndThen (結果に応じた計算の手作り: bind)" $ do
  it "calc 5 == Just 10" $
    calc 5 `shouldBe` Just 10
  it "calc 0 == Nothing (最初の割り算で失敗)" $
    calc 0 `shouldBe` Nothing
  it "つなげる関数しだいで分岐が変わる (assertNonNeg / halve)" $ do
    (safeDiv 100 4 `bind` assertNonNeg) `shouldBe` Just 25
    (safeDiv 100 4 `bind` halve) `shouldBe` Nothing
    (safeDiv 100 5 `bind` halve) `shouldBe` Just 10
  it "calc3: 3 段の連鎖 (後段が前段の結果 r を使う)" $ do
    calc3 100 5 `shouldBe` Just 30
    calc3 100 4 `shouldBe` Nothing
    calc3 100 0 `shouldBe` Nothing
  it "bind は join . fmap と一致する" $
    (Just 20 `bind` \r -> safeDiv r 2)
      `shouldBe` maybe Nothing id (fmap (\r -> safeDiv r 2) (Just 20))
