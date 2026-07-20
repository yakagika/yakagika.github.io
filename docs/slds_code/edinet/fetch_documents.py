"""EDINET API v2 から有価証券報告書の一覧と CSV データを取得する.

実行方法:
    uv run python fetch_documents.py

事前準備:
    EDINET の API キーを取得し, 環境変数 EDINET_API_KEY に設定するか,
    下の API_KEY に直接記入する (キーを記入したファイルは共有しないこと).

出力:
    data/documents_index.csv  取得した書類の索引 (docID, 証券コード, 企業名, 決算期末)
    data/edinet_zip/<docID>.zip  各書類の CSV 一式 (zip)

取得に失敗した日・書類はその場で諦めず, いったん失敗 queue に積んでおき,
期間全体を回し終えてからサーバを休ませ, リクエスト間隔を伸ばした上で
queue の中身だけ再トライする (MAX_RETRY_ROUNDS 回まで).
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

# サーバへの配慮: 間隔が短すぎるとサーバが応答を返さなくなることがある
REQUEST_WAIT = 1.0     # 通常時のリクエスト間隔 (秒)
MAX_RETRY_ROUNDS = 3   # 失敗分を再トライする回数
RETRY_PAUSE = 30.0     # 再トライ前にサーバを休ませる時間 (秒). ラウンドごとに倍増


def iter_dates(start: datetime.date, end: datetime.date):
    """start から end までの日付を 1 日ずつ返す."""
    day = start
    while day <= end:
        yield day
        day += datetime.timedelta(days=1)


def check_api_status(payload: dict) -> None:
    """body 内のエラー通知を確認する.

    EDINET API はキーが無効な場合など, HTTP 200 のまま body の JSON で
    エラーを返すことがある (この場合 results が無いだけで一見成功に見える).
    """
    status = payload.get("StatusCode", payload.get("metadata", {}).get("status"))
    if status in (None, 200, "200"):
        return
    message = payload.get("message", "")
    if status in (401, "401"):
        raise SystemExit(f"API キーが無効です: {message}")
    raise requests.RequestException(f"API エラー {status}: {message}")


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
    payload = resp.json()
    check_api_status(payload)
    return payload.get("results", [])


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
    # zip でなく JSON が返るのはエラー (無効キーなら中断, それ以外は再トライへ)
    if "application/json" in resp.headers.get("Content-Type", ""):
        check_api_status(resp.json())
        print(f"  取得失敗 {doc_id}: zip でなく JSON 応答")
        return False
    if resp.status_code != 200:
        print(f"  取得失敗 {doc_id}: HTTP {resp.status_code}")
        return False
    dest.write_bytes(resp.content)
    return True


def index_row(doc: dict, day: datetime.date) -> dict:
    """索引 CSV の 1 行分を作る."""
    return {
        "docID": doc["docID"],
        "edinetCode": doc.get("edinetCode"),
        "secCode": doc.get("secCode"),       # 証券コード (5 桁) - 以後の結合キー
        "filerName": doc.get("filerName"),
        "periodStart": doc.get("periodStart"),
        "periodEnd": doc.get("periodEnd"),   # 決算期末 - 株価の参照日に使う
        "docDescription": doc.get("docDescription"),
        "submitDate": day.isoformat(),
    }


def process_document(
    doc: dict,
    day: datetime.date,
    index_rows: list[dict],
    failed_docs: list[tuple[dict, datetime.date]],
    wait: float,
) -> None:
    """書類 1 件の zip を取得して索引に加える. 失敗したら queue に積む."""
    doc_id = doc["docID"]
    zip_path = ZIP_DIR / f"{doc_id}.zip"
    if not zip_path.exists():  # 再実行時は取得済みをスキップ
        try:
            ok = download_csv_zip(doc_id, zip_path)
        except requests.RequestException as e:
            print(f"  取得失敗 {doc_id}: {e}")
            ok = False
        time.sleep(wait)  # 成否によらず, リクエストしたら必ず待つ
        if not ok:
            failed_docs.append((doc, day))
            return
    index_rows.append(index_row(doc, day))


def process_day(
    day: datetime.date,
    index_rows: list[dict],
    failed_days: list[datetime.date],
    failed_docs: list[tuple[dict, datetime.date]],
    wait: float,
) -> None:
    """1 日分の一覧を取得し, 有価証券報告書を順に取得する. 失敗したら queue に積む."""
    try:
        results = fetch_document_list(day)
    except requests.RequestException as e:
        print(f"{day}: 一覧取得に失敗 ({e})")
        results = None
    time.sleep(wait)
    if results is None:
        failed_days.append(day)
        return
    reports = [doc for doc in results if is_annual_securities_report(doc)]
    print(f"{day}: 提出 {len(results)} 件中, 有価証券報告書 {len(reports)} 件")
    for doc in reports:
        process_document(doc, day, index_rows, failed_docs, wait)


def main() -> None:
    if not API_KEY:
        raise SystemExit(
            "API キーが設定されていません. 環境変数 EDINET_API_KEY を設定してください."
        )
    ZIP_DIR.mkdir(parents=True, exist_ok=True)

    index_rows: list[dict] = []
    failed_days: list[datetime.date] = []                    # 一覧取得に失敗した日の queue
    failed_docs: list[tuple[dict, datetime.date]] = []       # 本体取得に失敗した書類の queue

    # 1 巡目: 期間内を通しで取得する. 失敗はその場で諦めず queue に積むだけにする
    for day in iter_dates(START_DATE, END_DATE):
        process_day(day, index_rows, failed_days, failed_docs, REQUEST_WAIT)

    # 再トライ: サーバを休ませ, リクエスト間隔も倍に伸ばした上で queue の中身だけやり直す
    wait = REQUEST_WAIT
    pause = RETRY_PAUSE
    for round_no in range(1, MAX_RETRY_ROUNDS + 1):
        if not failed_days and not failed_docs:
            break
        wait *= 2
        print(
            f"再トライ {round_no}/{MAX_RETRY_ROUNDS}: "
            f"一覧 {len(failed_days)} 日分 + 書類 {len(failed_docs)} 件 "
            f"({pause:.0f} 秒休んでから, 間隔 {wait:.0f} 秒で実行)"
        )
        time.sleep(pause)
        pause *= 2

        retry_days, failed_days = failed_days, []
        retry_docs, failed_docs = failed_docs, []
        for day in retry_days:
            process_day(day, index_rows, failed_days, failed_docs, wait)
        for doc, day in retry_docs:
            process_document(doc, day, index_rows, failed_docs, wait)

    if failed_days or failed_docs:
        print(
            f"警告: 一覧 {len(failed_days)} 日分, 書類 {len(failed_docs)} 件が"
            "取得できませんでした. 時間をおいて再実行してください"
            " (取得済み分はスキップされるので, 足りない分だけ再取得されます)."
        )

    # 一覧 API は同じ書類を重複して返すことがあるため docID で重複排除する
    index = pd.DataFrame(index_rows)
    if not index.empty:
        index = index.drop_duplicates(subset="docID")
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    index.to_csv(DATA_DIR / "documents_index.csv", index=False, encoding="utf-8-sig")
    print(f"完了: {len(index)} 件 -> {DATA_DIR / 'documents_index.csv'}")


if __name__ == "__main__":
    main()
