# -*- coding: utf-8 -*-
"""
特別講義DS 補足B「X(Twitter) API によるデータの取得」の配布コード (2/2).

fetch_posts.py が出力した posts.csv (1 行 = 1 投稿, X の素直な列名) を, 補足C の
ichikawa_youtube.csv と同じ列構成 (posts_youtube_format.csv) に変換する. 変換後は
補足C の sentiment_analysis.py が INPUT_CSV の変更だけで動く.

変換規則 (集計の単位: 動画 1 本 → 検索ワード 1 語):
  video_id / title     ← query
  view_count           ← impression_count のワード単位合計
  like_count           ← like_count のワード単位合計
  comment_count        ← ワード単位の行数
  comment              ← text
  comment_like_count   ← like_count
  comment_published_at ← created_at

実行:
  python to_youtube_format.py        # posts.csv → posts_youtube_format.csv
"""

import pandas as pd

INPUT_CSV  = 'posts.csv'
OUTPUT_CSV = 'posts_youtube_format.csv'


def convert(df: pd.DataFrame) -> pd.DataFrame:
    """1 行 = 1 投稿の DataFrame を補足C と同じ列構成に変換する."""
    g = df.groupby('query')
    return pd.DataFrame({
        'video_id'            : df['query'],
        'title'               : df['query'],
        'view_count'          : g['impression_count'].transform('sum'),
        'like_count'          : g['like_count'].transform('sum'),
        'comment_count'       : g['query'].transform('size'),
        'comment'             : df['text'],
        'comment_like_count'  : df['like_count'],
        'comment_published_at': df['created_at'],
    })


def main():
    df = pd.read_csv(INPUT_CSV)
    converted = convert(df)
    converted.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
    print(f'{OUTPUT_CSV} に {len(converted)} 行 ({df["query"].nunique()} ワード) を保存しました.')


if __name__ == '__main__':
    main()
