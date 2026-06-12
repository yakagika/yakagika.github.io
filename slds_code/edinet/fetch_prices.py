"""証券コードから株価を取得し, 各決算期末時点の終値を記録する.

実行方法:
    uv run python fetch_prices.py

入力:  data/documents_index.csv (fetch_documents.py の出力)
出力:  data/prices.csv  (secCode, 年度, 株価)

ポイント:
    - 企業名で検索せず, 証券コードからティッカー (例 7203.T) を機械的に作る.
    - 決算期末が休日の場合があるので「期末日以前の直近営業日」の終値を使う.
"""

import time
from pathlib import Path

import pandas as pd
import yfinance as yf

DATA_DIR = Path(__file__).parent / "data"


def to_ticker(sec_code: str) -> str | None:
    """EDINET の証券コード (5 桁, 末尾 0) を Yahoo Finance のティッカーに変換する."""
    code = str(sec_code).strip().split(".")[0]
    if len(code) == 5 and code.endswith("0"):
        code = code[:4]
    if len(code) != 4:
        return None
    return f"{code}.T"


def close_asof(prices: pd.DataFrame, date: pd.Timestamp) -> float | None:
    """date 以前の直近営業日の終値を返す (期末日が休日でも欠損しない)."""
    upto = prices.loc[prices.index <= date]
    if upto.empty:
        return None
    return float(upto["Close"].iloc[-1])


def main() -> None:
    index = pd.read_csv(DATA_DIR / "documents_index.csv", dtype={"secCode": str})
    index = index.dropna(subset=["secCode", "periodEnd"])
    index["periodEnd"] = pd.to_datetime(index["periodEnd"])
    index["年度"] = index["periodEnd"].dt.year

    rows = []
    for sec_code, group in index.groupby("secCode"):
        ticker = to_ticker(sec_code)
        if ticker is None:
            print(f"スキップ (証券コード不正): {sec_code}")
            continue

        start = group["periodEnd"].min() - pd.Timedelta(days=14)
        end = group["periodEnd"].max() + pd.Timedelta(days=1)
        prices = yf.download(ticker, start=start, end=end,
                             progress=False, auto_adjust=False)
        time.sleep(0.2)  # レート制限への配慮
        if prices.empty:
            print(f"株価なし: {ticker} ({group['filerName'].iloc[0]})")
            continue
        if isinstance(prices.columns, pd.MultiIndex):  # yfinance >= 0.2 の形式に対応
            prices.columns = prices.columns.get_level_values(0)

        for _, row in group.iterrows():
            close = close_asof(prices, row["periodEnd"])
            if close is None:
                continue
            rows.append({"secCode": sec_code, "年度": row["年度"], "株価": close})
        print(f"OK {ticker}: {len(group)} 期")

    out = pd.DataFrame(rows)
    out.to_csv(DATA_DIR / "prices.csv", index=False, encoding="utf-8-sig")
    print(f"完了: {len(out)} 行 -> {DATA_DIR / 'prices.csv'}")


if __name__ == "__main__":
    main()
