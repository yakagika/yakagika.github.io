"""EDINET の CSV zip 群を解析し, 企業 × 年度の財務データ表 (wide 形式) を作る.

実行方法:
    uv run python build_financial_table.py

入力:
    data/documents_index.csv  (fetch_documents.py の出力)
    data/edinet_zip/<docID>.zip

出力:
    data/financials_wide.csv  1 行 = 1 企業 1 年度, 列 = 財務項目
"""

import io
import zipfile
from pathlib import Path

import pandas as pd

DATA_DIR = Path(__file__).parent / "data"
ZIP_DIR = DATA_DIR / "edinet_zip"

# 報告書には当期だけでなく過年度の値も入っている. 相対年度を数値に直す.
RELATIVE_MAP = {
    "当期": 0, "前期": -1, "前々期": -2, "三期前": -3, "四期前": -4,
    "当期末": 0, "前期末": -1, "前々期末": -2, "三期前時点": -3, "四期前時点": -4,
}

# コンテキスト ID の優先順位 (同じ項目 × 年度が複数行ある場合, 新しい報告を優先)
CONTEXT_PRIORITY = [
    "CurrentYearDuration", "CurrentYearInstant",
    "Prior1YearDuration", "Prior1YearInstant",
    "Prior2YearDuration", "Prior2YearInstant",
]


def read_edinet_csv(raw: bytes) -> pd.DataFrame:
    """EDINET の CSV (UTF-16LE, タブ区切り) を読み込む."""
    return pd.read_csv(io.BytesIO(raw), encoding="utf-16", sep="\t", quotechar='"')


def parse_zip(zip_path: Path, meta: pd.Series) -> pd.DataFrame:
    """1 つの zip から有価証券報告書本体の CSV を読み, 縦持ちで返す."""
    frames = []
    with zipfile.ZipFile(zip_path) as z:
        for name in z.namelist():
            # 報告書本体のファイルのみ対象 (監査報告書などを除く)
            if not name.endswith(".csv") or "jpcrp030000-asr" not in name:
                continue
            df = read_edinet_csv(z.read(name))
            df.columns = [str(c).strip().replace("﻿", "") for c in df.columns]
            frames.append(df)
    if not frames:
        return pd.DataFrame()

    df = pd.concat(frames, ignore_index=True)

    # 決算期末から「当期」の年度を決め, 相対年度を絶対年度に変換する
    base_year = pd.Timestamp(meta["periodEnd"]).year
    offset = df["相対年度"].map(RELATIVE_MAP)
    df = df[offset.notna()].copy()
    df["年度"] = base_year + offset[offset.notna()].astype(int)

    df["値_数値"] = pd.to_numeric(
        df["値"].astype(str).str.replace(",", ""), errors="coerce"
    )
    df["secCode"] = meta["secCode"]
    df["企業名"] = meta["filerName"]
    return df[["secCode", "企業名", "年度", "項目名", "コンテキストID", "連結・個別", "値_数値"]]


def pick_one(group: pd.DataFrame) -> pd.Series:
    """同じ (企業, 年度, 項目) に複数行ある場合, 連結 > コンテキスト優先順で 1 行選ぶ."""
    g = group.copy()
    g["連結優先"] = g["連結・個別"].fillna("").str.contains("連結").astype(int)
    g["ctx優先"] = g["コンテキストID"].map(
        lambda x: next(
            (i for i, k in enumerate(CONTEXT_PRIORITY) if isinstance(x, str) and k in x),
            len(CONTEXT_PRIORITY),
        )
    )
    g = g.sort_values(["連結優先", "ctx優先"], ascending=[False, True])
    return g.iloc[0]


def main() -> None:
    index = pd.read_csv(DATA_DIR / "documents_index.csv", dtype={"secCode": str})
    index = index.dropna(subset=["secCode"])  # 上場企業 (証券コードあり) に限定

    long_frames = []
    for _, meta in index.iterrows():
        zip_path = ZIP_DIR / f"{meta['docID']}.zip"
        if not zip_path.exists():
            continue
        try:
            df = parse_zip(zip_path, meta)
        except (zipfile.BadZipFile, KeyError, ValueError) as e:
            print(f"解析失敗 {meta['docID']} ({meta['filerName']}): {e}")
            continue
        if not df.empty:
            long_frames.append(df)
            print(f"OK {meta['filerName']}: {len(df)} 行")

    long_df = pd.concat(long_frames, ignore_index=True)

    # 重複の解決: 連結優先 + コンテキスト優先で 1 行に絞る
    dedup = (
        long_df.groupby(["secCode", "企業名", "年度", "項目名"], as_index=False)
        .apply(pick_one, include_groups=False)
        .reset_index(drop=True)
    )

    # wide 化: 1 行 = 1 企業 1 年度. 欠損はそのまま NaN にしておく
    wide = dedup.pivot_table(
        index=["secCode", "企業名", "年度"],
        columns="項目名",
        values="値_数値",
        aggfunc="first",
    ).reset_index().rename_axis(None, axis=1)

    wide.to_csv(DATA_DIR / "financials_wide.csv", index=False, encoding="utf-8-sig")
    print(f"完了: {len(wide)} 行 (企業 {wide['secCode'].nunique()} 社)")


if __name__ == "__main__":
    main()
