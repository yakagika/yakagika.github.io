"""財務指標・株価・業種・(任意で) ESG スコアを証券コードで結合し, 分析用データを作る.

実行方法:
    uv run python merge_all.py

入力:
    data/financial_indicators.csv  (compute_indicators.py の出力)
    data/prices.csv                (fetch_prices.py の出力)
    data/jpx_industry.csv          (JPX 公表の銘柄一覧から作成: コード, 33業種区分)
    data/esg_scores.csv            (任意: secCode, 年度, ESGスコア の 3 列)

出力:
    data/analysis_table.csv

ポイント:
    - 結合キーはすべて証券コード (secCode). 企業名の文字列では結合しない
      (表記ゆれ ((株), 全角/半角, 中黒など) で取りこぼすため).
"""

from pathlib import Path

import numpy as np
import pandas as pd

DATA_DIR = Path(__file__).parent / "data"

# 33 業種区分 -> 大分類 (サンプルサイズ確保のため粗くする)
MAJOR_INDUSTRY = {
    "水産・農林業": "水産・農林業", "鉱業": "鉱業", "建設業": "建設業",
    "食料品": "製造業", "繊維製品": "製造業", "パルプ・紙": "製造業",
    "化学": "製造業", "医薬品": "製造業", "石油・石炭製品": "製造業",
    "ゴム製品": "製造業", "ガラス・土石製品": "製造業", "鉄鋼": "製造業",
    "非鉄金属": "製造業", "金属製品": "製造業", "機械": "製造業",
    "電気機器": "製造業", "輸送用機器": "製造業", "精密機器": "製造業",
    "その他製品": "製造業",
    "電気・ガス業": "電気・ガス業",
    "陸運業": "運輸・情報通信業", "海運業": "運輸・情報通信業",
    "空運業": "運輸・情報通信業", "倉庫・運輸関連業": "運輸・情報通信業",
    "情報・通信業": "運輸・情報通信業",
    "卸売業": "商業", "小売業": "商業",
    "銀行業": "金融・保険業", "証券、商品先物取引業": "金融・保険業",
    "保険業": "金融・保険業", "その他金融業": "金融・保険業",
    "不動産業": "不動産業", "サービス業": "サービス業",
}


def normalize_sec_code(s: pd.Series) -> pd.Series:
    """証券コードを 4 桁の文字列に揃える (EDINET は 5 桁, JPX は 4 桁)."""
    code = s.astype(str).str.strip().str.split(".").str[0]
    return code.where(code.str.len() != 5, code.str[:4])


def main() -> None:
    fin = pd.read_csv(DATA_DIR / "financial_indicators.csv", dtype={"secCode": str})
    prices = pd.read_csv(DATA_DIR / "prices.csv", dtype={"secCode": str})
    industry = pd.read_csv(DATA_DIR / "jpx_industry.csv", dtype=str)

    fin["code4"] = normalize_sec_code(fin["secCode"])
    prices["code4"] = normalize_sec_code(prices["secCode"])
    industry["code4"] = normalize_sec_code(industry["コード"])
    industry["産業大分類"] = industry["33業種区分"].map(MAJOR_INDUSTRY)

    df = fin.merge(prices[["code4", "年度", "株価"]], on=["code4", "年度"], how="left")
    df = df.merge(industry[["code4", "33業種区分", "産業大分類"]], on="code4", how="left")

    # ESG スコアは商用データのため同梱していない. 各自入手したものを
    # data/esg_scores.csv (secCode, 年度, ESGスコア) として置けば結合される.
    esg_path = DATA_DIR / "esg_scores.csv"
    if esg_path.exists():
        esg = pd.read_csv(esg_path, dtype={"secCode": str})
        esg["code4"] = normalize_sec_code(esg["secCode"])
        df = df.merge(esg[["code4", "年度", "ESGスコア"]], on=["code4", "年度"], how="left")
        print(f"ESG スコアを結合: {df['ESGスコア'].notna().sum()} 行に値あり")
    else:
        print("ESG スコアなしで作成 (data/esg_scores.csv を置けば結合されます)")

    df["PBR"] = df["株価"] / df["BPS"].replace(0, np.nan)

    # 欠損はここでは消さない (どの変数を使うかは分析側で決める)
    df = df.drop(columns=["code4"]).sort_values(["secCode", "年度"])
    df.to_csv(DATA_DIR / "analysis_table.csv", index=False, encoding="utf-8-sig")

    print(f"完了: {len(df)} 行")
    print("結合状況:")
    for col in ["株価", "産業大分類"]:
        print(f"  {col}: {df[col].notna().sum()}/{len(df)} 行")


if __name__ == "__main__":
    main()
