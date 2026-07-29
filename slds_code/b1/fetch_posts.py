# -*- coding: utf-8 -*-
"""
特別講義DS 補足B「X(Twitter) API によるデータの取得」の配布コード.

検索ワードごとに X API v2 の recent search (直近 7 日) で投稿を取得し, 1 行 = 1 投稿の
CSV (posts.csv) にまとめる. 列名は X の投稿の意味そのまま (query, text, like_count, ...)。
補足C の sentiment_analysis.py を流用する場合は, to_youtube_format.py で補足C と同じ
列構成 (posts_youtube_format.csv) に変換してから使う.

X API は 2026-02 の改定で従量課金のみになった (投稿の取得 = 1 件 0.005 USD). 取得件数が
そのまま費用になるので, 実行前に必ず --dry-run で件数と概算費用を確認すること.

事前準備:
  uv add requests pandas
  export X_BEARER_TOKEN=...          # 教員から配布された Bearer Token
実行:
  python fetch_posts.py --dry-run    # 取得計画と概算費用だけ表示 (トークン不要, 課金なし)
  python fetch_posts.py              # → posts.csv (実行前に確認プロンプトが出る)
"""

import argparse
import os
import sys

import pandas as pd
import requests

SEARCH_URL         = 'https://api.x.com/2/tweets/search/recent'
QUERIES            = ['国民民主党', '自民党']   # 検索ワード (1 語につき 1 リクエスト)
MAX_RESULTS        = 50                        # 1 語あたりの取得件数
HARD_CAP           = 100                       # 1 語あたりの上限 (安全装置)
PRICE_PER_POST_USD = 0.005                     # 投稿 1 件あたりの単価 (2026-02 改定)
OUTPUT_CSV         = 'posts.csv'

BEARER_TOKEN = os.environ.get('X_BEARER_TOKEN', '')


def clamp(n: int) -> int:
    """1 リクエストあたりの取得件数を API の範囲 (10〜100) と HARD_CAP に収める."""
    return max(10, min(int(n), HARD_CAP))


def estimate(queries: list, max_results: int) -> tuple:
    """取得予定件数と概算費用 (USD) を返す."""
    total = len(queries) * clamp(max_results)
    return total, total * PRICE_PER_POST_USD


def print_plan(queries: list, max_results: int) -> None:
    """課金の前に, 何件取得していくらかかるかを表示する."""
    total, cost = estimate(queries, max_results)
    print('=== 取得計画 ===')
    print(f'検索ワード   : {", ".join(queries)} ({len(queries)} 語)')
    print(f'1 語あたり   : {clamp(max_results)} 件 (上限 {HARD_CAP} 件)')
    print(f'合計取得件数 : {total} 件')
    print(f'概算費用     : {total} 件 x ${PRICE_PER_POST_USD} = ${cost:.2f}')


def bearer_oauth(r):
    """Bearer Token 認証に必要なヘッダーを付ける."""
    r.headers['Authorization'] = f'Bearer {BEARER_TOKEN}'
    r.headers['User-Agent'] = 'v2RecentSearchPython'
    return r


def connect_to_endpoint(url: str, params: dict) -> dict:
    """GET リクエストを送り, レスポンスの JSON を返す."""
    response = requests.get(url, auth=bearer_oauth, params=params)
    response.encoding = response.apparent_encoding
    if response.status_code != 200:
        # 401 = トークンが誤っている, 403 = 残高不足, 429 = リクエスト過多
        raise RuntimeError(f'{response.status_code}: {response.text}')
    return response.json()


def fetch_posts(query: str, max_results: int) -> list:
    """検索ワード 1 語ぶんの投稿を取得し, JSON の data 部分を返す."""
    params = {
        'query'       : f'{query} lang:ja -is:retweet',   # 日本語かつリツイート以外
        'max_results' : clamp(max_results),
        'tweet.fields': 'created_at,public_metrics,author_id,lang',
    }
    return connect_to_endpoint(SEARCH_URL, params).get('data', [])


def to_row(query: str, post: dict) -> dict:
    """投稿 1 件を CSV の 1 行に変換する."""
    m = post.get('public_metrics', {})
    return {
        'query'           : query,                     # 検索ワード
        'text'            : post.get('text', ''),      # 投稿本文
        'like_count'      : int(m.get('like_count', 0)),
        'retweet_count'   : int(m.get('retweet_count', 0)),
        'reply_count'     : int(m.get('reply_count', 0)),
        'impression_count': int(m.get('impression_count', 0)),   # 取れない場合は 0
        'created_at'      : post.get('created_at', ''),
        'post_id'         : post.get('id', ''),
        'author_id'       : post.get('author_id', ''),
    }


def main():
    parser = argparse.ArgumentParser(
        description='X API v2 で投稿を取得し, 1 行 = 1 投稿の CSV にまとめる.')
    parser.add_argument('--dry-run', action='store_true',
                        help='取得計画と概算費用だけ表示して終了する (トークン不要, 課金なし)')
    parser.add_argument('--yes', action='store_true',
                        help='実行前の確認プロンプトを省略する')
    args = parser.parse_args()

    print_plan(QUERIES, MAX_RESULTS)

    if args.dry_run:
        print('--dry-run のため API にリクエストを送らずに終了します.')
        return

    if not BEARER_TOKEN:
        print('環境変数 X_BEARER_TOKEN が設定されていません.', file=sys.stderr)
        print("  export X_BEARER_TOKEN='配布されたトークン'", file=sys.stderr)
        sys.exit(1)

    if not args.yes and input('この内容で取得しますか? [y/N]: ').strip().lower() != 'y':
        print('中止しました.')
        return

    rows = []
    for query in QUERIES:
        print(f'{query} の投稿を取得中...')
        for post in fetch_posts(query, MAX_RESULTS):
            rows.append(to_row(query, post))

    if not rows:
        print('取得できた投稿がありません.')
        return

    df = pd.DataFrame(rows)
    df.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
    print(f'{OUTPUT_CSV} に {len(df)} 行 ({df["query"].nunique()} ワード) を保存しました.')
    print(f'実際の取得件数 {len(df)} 件 = 約 ${len(df) * PRICE_PER_POST_USD:.2f}')


if __name__ == '__main__':
    main()
