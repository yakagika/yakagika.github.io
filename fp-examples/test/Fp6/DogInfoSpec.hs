-- | fp6.md 「直積型 レコード構文」節の warn ブロック内 `DogInfo`.
-- 直和型 + レコード構文でアクセサが部分関数になる例.
module Fp6.DogInfoSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

data MyDogs = GoldenRetriever | BlackRetriever | ShetlandSheepdog
            | StandardPoodle | Beagle
            deriving (Show, Eq)

data DogInfo = JustBreed { dogBreed :: MyDogs }
             | WithAge   { dogBreed :: MyDogs, dogAge :: Int }
             deriving (Show, Eq)

spec :: Spec
spec = describe "Fp6.DogInfo (部分アクセサ)" $ do
  it "dogBreed (JustBreed GoldenRetriever) は両コンストラクタにあるので OK" $
    dogBreed (JustBreed GoldenRetriever) `shouldBe` GoldenRetriever
  it "dogBreed (WithAge Beagle 7) も OK" $
    dogBreed (WithAge Beagle 7) `shouldBe` Beagle
  it "dogAge (WithAge Beagle 7) == 7" $
    dogAge (WithAge Beagle 7) `shouldBe` 7
  it "dogAge (JustBreed ...) は実行時エラー (部分関数)" $
    -- GHC のレコードセレクタ部分関数エラーは ErrorCall ではなく
    -- RecSelError 系の例外なので anyException で捕捉する.
    evaluate (dogAge (JustBreed GoldenRetriever))
      `shouldThrow` anyException
