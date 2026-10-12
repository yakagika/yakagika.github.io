{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- 教材では書き漏れの警告を見せるために, わざと場合分けを尽くしていない.
module Fp1.SplitMethodSpec (spec) where

import Test.Hspec

-- 事業所の調査票で, 副業に当たるサービス業の売上が把握されているか
data EstablishmentSales
    = SalesReported
    | SalesMissing
    deriving (Show)

-- 企業の調査票で, 生産物別の売上が把握されているか
data CompanyBreakdown
    = BreakdownReported
    | BreakdownMissing
    deriving (Show)

-- 売上を分けるときに使う構成比
data SplitMethod
    = ByCompanyBreakdown -- 企業の調査票の構成比
    | ByIndustryAverage  -- 別に集計した構成比
    deriving (Show, Eq)

splitMethod :: EstablishmentSales -> CompanyBreakdown -> SplitMethod
splitMethod SalesReported BreakdownReported = ByCompanyBreakdown
splitMethod SalesReported BreakdownMissing  = ByIndustryAverage

spec :: Spec
spec = describe "Fp1.SplitMethod (コラム: 統計の作成手順)" $ do
  it "事業所の売上と企業の内訳がともに把握されていれば企業の構成比" $
    splitMethod SalesReported BreakdownReported `shouldBe` ByCompanyBreakdown
  it "企業の内訳が把握されなければ別に集計した構成比" $
    splitMethod SalesReported BreakdownMissing `shouldBe` ByIndustryAverage
