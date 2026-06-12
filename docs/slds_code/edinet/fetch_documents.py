"""EDINET API v2 から有価証券報告書の一覧と CSV データを取得する.

実行方法:
    uv run python fetch_documents.py

事前準備:
    EDINET の API キーを取得し, 環境変数 EDINET_API_KEY に設定するか,
    下の API_KEY に直接記入する (キーを記入したファイルは共有しないこと).

出力:
    data/documents_index.csv  取得した書類の索引 (docID, 証券コード, 企業名, 決算期末)
    data/edinet_zip/<docID>.zip  各書類の CSV 一式 (zip)
"""

import datetime
import os
import time
from pathlib import Path

import pandas as pd
import requests

# ===== 設定 =====
API_KEY = os.environ.get("EDINET_API_KEY", "")  # 各自取得したキーを設定する
START_DATE = datetime.date(2025, 6, 1)   # 取得開始日 (提出日ベース)
END_DATE = datetime.date(2025, 6, 30)    # 取得終了日

BASE_URL = "https://api.edinet-fsa.go.jp/api/v2"
DATA_DIR = Path(__file__).parent / "data"
ZIP_DIR = DATA_DIR / "edinet_zip"

# 有価証券報告書 (内国会社) の指定: 開示府令 (010) の様式 030000
TARGET_ORDINANCE = "010"
TARGET_FORM = "030000"


def iter_dates(start: datetime.date, end: datetime.date):
    """start から end までの日付を 1 日ずつ返す."""
    day = start
    while day <= end:
        yield day
        day += datetime.timedelta(days=1)


def fetch_document_list(day: datetime.date) -> list[dict]:
    """書類一覧 API (type=2) で 1 日分の提出書類メタデータを取得する."""
    resp = requests.get(
        f"{BASE_URL}/documents.json",
        params={
            "date": day.isoformat(),
            "type": 2,  # 2 = 提出書類一覧およびメタデータ
            "Subscription-Key": API_KEY,
        },
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json().get("results", [])


def is_annual_securities_report(doc: dict) -> bool:
    """有価証券報告書 (内国会社) かどうかを様式コードで判定する."""
    return (
        doc.get("ordinanceCode") == TARGET_ORDINANCE
        and doc.get("formCode") == TARGET_FORM
        and doc.get("csvFlag") == "1"  # CSV が提供されている書類のみ
    )


def download_csv_zip(doc_id: str, dest: Path) -> bool:
    """書類取得 API (type=5) で CSV 一式の zip を保存する."""
    resp = requests.get(
        f"{BASE_URL}/documents/{doc_id}",
        params={"type": 5, "Subscription-Key": API_KEY},  # 5 = CSV
        timeout=60,
    )
    if resp.status_code != 200:
        print(f"  取得失敗 {doc_id}: HTTP {resp.status_code}")
        return False
    dest.write_bytes(resp.content)
    return True


def main() -> None:
    if not API_KEY:
        raise SystemExit(
            "API キーが設定されていません. 環境変数 EDINET_API_KEY を設定してください."
        )
    ZIP_DIR.mkdir(parents=True, exist_ok=True)

    index_rows = []
    for day in iter_dates(START_DATE, END_DATE):
        try:
            results = fetch_document_list(day)
        except requests.RequestException as e:
            print(f"{day}: 一覧取得に失敗 ({e}) - スキップ")
            continue

        reports = [doc for doc in results if is_annual_securities_report(doc)]
        print(f"{day}: 提出 {len(results)} 件中, 有価証券報告書 {len(reports)} 件")

        for doc in reports:
            doc_id = doc["docID"]
            zip_path = ZIP_DIR / f"{doc_id}.zip"
            if not zip_path.exists():  # 再実行時は取得済みをスキップ
                if not download_csv_zip(doc_id, zip_path):
                    continue
                time.sleep(0.2)  # サーバに負荷をかけない
            index_rows.append(
                {
                    "docID": doc_id,
                    "edinetCode": doc.get("edinetCode"),
                    "secCode": doc.get("secCode"),       # 証券コード (5 桁) - 以後の結合キー
                    "filerName": doc.get("filerName"),
                    "periodStart": doc.get("periodStart"),
                    "periodEnd": doc.get("periodEnd"),   # 決算期末 - 株価の参照日に使う
                    "docDescription": doc.get("docDescription"),
                    "submitDate": day.isoformat(),
                }
            )

    # 一覧 API は同じ書類を重複して返すことがあるため docID で重複排除する
    index = pd.DataFrame(index_rows).drop_duplicates(subset="docID")
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    index.to_csv(DATA_DIR / "documents_index.csv", index=False, encoding="utf-8-sig")
    print(f"完了: {len(index)} 件 -> {DATA_DIR / 'documents_index.csv'}")


if __name__ == "__main__":
    main()
