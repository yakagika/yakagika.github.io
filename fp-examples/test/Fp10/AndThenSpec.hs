-- | fp10.md 「依存する連鎖を手作りする — andThen」節のコード例.
--   andThen = Maybe 専用の >>= の手作り版 (join . fmap の Maybe 形).
module Fp10.AndThenSpec (spec) where

import Test.Hspec

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing  _ = Nothing
andThen (Just x) f = f x

calc :: Int -> Maybe Int
calc x = safeDiv 100 x `andThen` \r -> safeDiv r 2

spec :: Spec
spec = describe "Fp10.AndThen (依存する連鎖の手作り)" $ do
  it "calc 5 == Just 10" $
    calc 5 `shouldBe` Just 10
  it "calc 0 == Nothing (最初の割り算で失敗)" $
    calc 0 `shouldBe` Nothing
  it "andThen は join . fmap と一致する" $
    (Just 20 `andThen` \r -> safeDiv r 2)
      `shouldBe` maybe Nothing id (fmap (\r -> safeDiv r 2) (Just 20))
