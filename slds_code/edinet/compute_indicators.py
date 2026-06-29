"""財務データ表から分析用の財務指標を計算する.

実行方法:
    uv run python compute_indicators.py

入力:  data/financials_wide.csv (build_financial_table.py の出力)
出力:  data/financial_indicators.csv
"""

from pathlib import Path

import numpy as np
import pandas as pd

DATA_DIR = Path(__file__).parent / "data"

# EDINET の項目名は提出企業・年度によって揺れるため, 優先順位つきで 1 つに集約する
# (coalesce: 先頭から順に探し, 最初に見つかった非欠損値を使う)
COALESCE = {
    "当期純利益": [
        "当期純利益又は当期純損失（△）（平成26年3月28日財規等改正後）",
        "当期純利益又は当期純損失（△）、経営指標等",
    ],
    "総資産": ["総資産額、経営指標等"],
    "純資産": ["純資産","純資産額、経営指標等"],
    "売上高": ["売上高、経営指標等"],
    "売上総利益": ["売上総利益又は売上総損失（△）", "売上総利益（IFRS）"],
    "営業利益": ["営業利益又は営業損失（△）"],
    "経常利益": ["経常利益又は経常損失（△）", "経常利益又は経常損失（△）、経営指標等"],
    "営業外収益": ["営業外収益"],
    "営業外費用": ["営業外費用"],
    "BPS": ["１株当たり純資産額、経営指標等"],
    "発行済株式総数": ["発行済株式総数（普通株式）、経営指標等"],
}


def coalesce(df: pd.DataFrame, candidates: list[str]) -> pd.Series:
    """候補列を先頭から順に見て, 最初の非欠損値を採用した Series を返す."""
    result = pd.Series(np.nan, index=df.index, dtype=float)
    for col in candidates:
        if col in df.columns:
            result = result.fillna(pd.to_numeric(df[col], errors="coerce"))
    return result


def safe_div(a: pd.Series, b: pd.Series) -> pd.Series:
    """0 割りを NaN にする除算."""
    return a / b.replace(0, np.nan)


def main() -> None:
    df = pd.read_csv(DATA_DIR / "financials_wide.csv", dtype={"secCode": str})

    base = df[["secCode", "企業名", "年度"]].copy()
    v = {name: coalesce(df, cols) for name, cols in COALESCE.items()}

    # 経常利益が直接取れない場合の補完: 経常利益 = 営業利益 + 営業外収益 - 営業外費用
    keijo = v["経常利益"].fillna(v["営業利益"] + v["営業外収益"] - v["営業外費用"])

    base["ROA (総資産利益率)"] = safe_div(v["当期純利益"], v["総資産"]) * 100
    base["売上高総利益率"] = safe_div(v["売上総利益"], v["売上高"]) * 100
    base["営業利益率"] = safe_div(v["営業利益"], v["売上高"]) * 100
    base["総資産回転率"] = safe_div(v["売上高"], v["総資産"])
    base["自己資本比率"] = safe_div(v["純資産"], v["総資産"]) * 100
    base["売上高経常利益率"] = safe_div(keijo, v["売上高"]) * 100
    base["資本生産性"] = safe_div(v["売上高"], v["純資産"])
    base["BPS"] = v["BPS"]
    base["発行済株式総数"] = v["発行済株式総数"]

    # 比率が定義域を明らかに外れる行は記録した上で NaN にする
    out_of_range = base["売上高総利益率"] > 100
    if out_of_range.any():
        print(f"注意: 売上高総利益率 > 100% が {out_of_range.sum()} 行 (NaN に置換)")
        base.loc[out_of_range, "売上高総利益率"] = np.nan

    base.to_csv(DATA_DIR / "financial_indicators.csv", index=False, encoding="utf-8-sig")
    print(f"完了: {len(base)} 行 -> {DATA_DIR / 'financial_indicators.csv'}")


if __name__ == "__main__":
    main()
