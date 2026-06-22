---
title: 特別講義DS 補足C YouTube Data APIによる動画・チャンネルデータの取得
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: false
date: 2026-06-21
tableOfContents: true
previousChapter: slds_b1.html
---

本資料は章番号外の補足資料です. Google が提供する **YouTube Data API v3** を利用して, 動画の統計情報 (再生数・高評価数・コメント数) やチャンネル情報を取得し, pandas DataFrame に整形するまでの流れを扱います.

取得したデータを使ったテキスト分析 (タイトル・説明欄のワードクラウドやトピックモデル) は [Ch15 自然言語処理](slds15.html) で扱っている手法が応用できます. また動画の時系列統計を使った回帰分析は [Ch11 線形回帰分析](slds11.html) が参考になります.

API という仕組み自体の説明 (REST, HTTP メソッド, JSON など) は[補足A](slds_a1.html#apiとは)にまとめてあるので, 馴染みのない人は先にそちらを読んでください. YouTube Data API も `REST` アーキテクチャで提供されており, 本資料では `GET` メソッドだけを使います.

関連する章・補足:

- [補足A EDINET APIによる財務データの取得](slds_a1.html) — API の基礎 (REST/HTTP/JSON) の説明と API キーの取り扱い方
- [補足B X(Twitter) APIによるデータの取得](slds_b1.html) — 別の REST API 事例. X との料金・制限の比較は[後の節](#quota制限とxとの比較)で扱います
- [Ch15 自然言語処理](slds15.html) — 取得したタイトル・説明欄テキストの分析に利用できます

# YouTube Data API v3 とは

**YouTube Data API v3** は, Google が公式に提供する REST API で, 動画・チャンネル・再生リスト・コメントなどの情報をプログラムから取得・操作できます. データサイエンスの観点では特に以下のリソースが有用です.

| リソース | 取得できる主な情報 |
|---|---|
| `videos` | 再生数, 高評価数, コメント数, 投稿日時, タグ, 説明欄 |
| `channels` | 登録者数, 総再生数, 動画本数, チャンネル概要 |
| `search` | キーワード・チャンネル・期間などを条件に動画を検索して ID を取得 |
| `commentThreads` | 動画に付いたコメントとそのメタデータ |

本資料では (a) チャンネル統計の取得, (b) キーワード検索による動画一覧の取得, (c) 動画ごとの再生数等の統計取得 という3 段階の worked example を示します.

::: note
YouTube Data API v3 は **無料の quota (割り当て)** の範囲内で利用できます. 1 日あたり **10,000 ユニット** (クォータ単位) が無料で付与されており, 通常の学習・研究用途では十分です. 詳細は [後の節](#quota制限とxとの比較) を参照してください.
:::

# API キーの取得

YouTube Data API の利用に必要な認証方式は **API キー** (公開データの読み取りのみ) と **OAuth 2.0** (ログインユーザ固有のデータ操作) の 2 種類があります. 本資料では公開されている動画・チャンネルデータを取得するだけなので, 手続きが簡単な **API キー** を使います.

API キーの取得手順については [補足A の「API キーの取得」節](slds_a1.html#apiキーの取得) で Google Cloud Console を使った手順が説明されています. 以下では YouTube Data API に特有の部分だけを補足します.

1. [Google Cloud Console](https://console.cloud.google.com/) にアクセスし, プロジェクトを作成 (または既存のものを選択) する.
2. 左メニューの **「API とサービス」→「ライブラリ」** から **「YouTube Data API v3」** を検索して有効化する.
3. **「API とサービス」→「認証情報」→「認証情報を作成」→「API キー」** を選択する.
4. 作成された API キーをコピーして安全な場所に保管する.

::: note
API キーは[補足A](slds_a1.html#apiキーの取得) で解説しているとおり**パスワードと同じ扱い**をしてください. ソースコードに直接書いたまま提出・共有・公開すると漏洩します. 環境変数やコード外ファイルへの分離を推奨します.
:::

# 取得プログラム

必要なライブラリをインストールします. 公式の `google-api-python-client` を使うと YouTube Data API の構造に沿った簡潔なコードが書けますが, 補足B と同様に `requests` だけで書くこともできます. 本資料では両方を示しますが, **まず `google-api-python-client` 版を推奨します**.

~~~ sh
uv add google-api-python-client pandas
~~~

## (a) チャンネル統計の取得

指定したチャンネルの登録者数・総再生数・動画本数を取得します.

~~~ py
from googleapiclient.discovery import build
import pandas as pd

# API キー (環境変数や外部ファイルから読み込むことを推奨)
API_KEY = 'YOUR_API_KEY'

# YouTube Data API クライアントの作成
youtube = build('youtube', 'v3', developerKey=API_KEY)

def get_channel_stats(channel_id: str) -> dict:
    """
    チャンネル ID を受け取り, 統計情報 (登録者数・総再生数・動画本数) を辞書で返す.

    Parameters
    ----------
    channel_id : str
        YouTube チャンネル ID (例: "UCxxxxxx")

    Returns
    -------
    dict
        チャンネル名, 登録者数, 総再生数, 動画本数を含む辞書
    """
    request = youtube.channels().list(
        part='snippet,statistics',
        id=channel_id
    )
    response = request.execute()

    if not response.get('items'):
        raise ValueError(f"チャンネルが見つかりませんでした: {channel_id}")

    item = response['items'][0]
    stats = item['statistics']
    snippet = item['snippet']

    return {
        'channel_id'       : channel_id,
        'channel_name'     : snippet['title'],
        'subscriber_count' : int(stats.get('subscriberCount', 0)),
        'view_count'       : int(stats.get('viewCount', 0)),
        'video_count'      : int(stats.get('videoCount', 0)),
    }

# 例: NHK のチャンネル (channel_id は実際の ID に置き換えてください)
channel_id = 'UCXXXXXXXXXXXXXXXXXXXXXXX'  # 例: 'UC6o44hS0ze1blFN3TQEPV3w' (NHK)

stats = get_channel_stats(channel_id)
df_channel = pd.DataFrame([stats])
print(df_channel)
~~~

::: note
**チャンネル ID の調べ方**: YouTube でチャンネルを開いたとき URL の `@xxxx` 部分はハンドルであり, チャンネル ID (`UCxxxxxx` 形式) とは異なる場合があります. チャンネルページ → 「概要」→「チャンネルを共有」→ 「チャンネル ID をコピー」で確認できます. または `search` エンドポイントでチャンネル名から検索する方法もあります (Quota を 100 ユニット消費します).
:::

## (b) キーワード検索による動画一覧の取得

「データサイエンス 入門」などのキーワードで動画を検索し, 動画 ID・タイトル・投稿日を一覧化します.

~~~ py
def search_videos(query: str, max_results: int = 10) -> pd.DataFrame:
    """
    キーワードで YouTube 動画を検索し, 動画 ID・タイトル・投稿日の DataFrame を返す.

    Parameters
    ----------
    query : str
        検索キーワード
    max_results : int
        取得する動画の最大件数 (上限 50, デフォルト 10)

    Returns
    -------
    pd.DataFrame
        video_id, title, published_at を列に持つ DataFrame
    """
    request = youtube.search().list(
        part='snippet',
        q=query,
        type='video',         # 動画のみ (チャンネル・再生リストを除外)
        maxResults=max_results,
        relevanceLanguage='ja'  # 日本語優先
    )
    response = request.execute()

    rows = []
    for item in response.get('items', []):
        rows.append({
            'video_id'    : item['id']['videoId'],
            'title'       : item['snippet']['title'],
            'published_at': item['snippet']['publishedAt'],
        })

    return pd.DataFrame(rows)

# 例: 「データサイエンス 入門」で検索
df_search = search_videos('データサイエンス 入門', max_results=10)
print(df_search)
~~~

::: note
`search().list()` は 1 回の呼び出しで **100 ユニット** を消費します. 無料枠 (1 日 10,000 ユニット) の中では 100 回まで検索できますが, ループ中で大量に呼ぶと quota がすぐ尽きるので注意してください.
:::

## (c) 動画ごとの統計 (再生数・高評価数・コメント数) の取得

`(b)` で得た動画 ID のリストを使って, 各動画の再生数・高評価数・コメント数を取得します. `videos().list()` は **1 回で最大 50 件**の動画 ID を渡せるので, バッチ処理でまとめて取得すると quota を節約できます.

~~~ py
def get_video_stats(video_ids: list[str]) -> pd.DataFrame:
    """
    動画 ID のリストを受け取り, 各動画の統計情報を DataFrame で返す.
    50 件を超える場合は自動的に複数回に分けてリクエストする.

    Parameters
    ----------
    video_ids : list[str]
        動画 ID のリスト

    Returns
    -------
    pd.DataFrame
        video_id, title, view_count, like_count, comment_count, published_at を列に持つ DataFrame
    """
    rows = []
    # YouTube API は 1 リクエストあたり最大 50 件
    chunk_size = 50
    for i in range(0, len(video_ids), chunk_size):
        chunk = video_ids[i:i + chunk_size]
        request = youtube.videos().list(
            part='snippet,statistics',
            id=','.join(chunk)
        )
        response = request.execute()

        for item in response.get('items', []):
            stats   = item.get('statistics', {})
            snippet = item.get('snippet', {})
            rows.append({
                'video_id'     : item['id'],
                'title'        : snippet.get('title', ''),
                'published_at' : snippet.get('publishedAt', ''),
                'view_count'   : int(stats.get('viewCount', 0)),
                'like_count'   : int(stats.get('likeCount', 0)),
                'comment_count': int(stats.get('commentCount', 0)),
            })

    return pd.DataFrame(rows)

# (b) の検索結果を使って動画統計を取得
video_ids = df_search['video_id'].tolist()
df_stats = get_video_stats(video_ids)

# 再生数の多い順に並べ替え
df_stats_sorted = df_stats.sort_values('view_count', ascending=False)
print(df_stats_sorted[['title', 'view_count', 'like_count', 'comment_count']])
~~~

## まとめ: 取得→分析のパイプライン

上の 3 つのステップをつなぐと, **キーワード検索 → 動画統計取得 → DataFrame 化 → CSV 保存** という一連のパイプラインになります.

~~~ py
import os
from googleapiclient.discovery import build
import pandas as pd

# ---- 設定 ----
API_KEY    = os.environ.get('YOUTUBE_API_KEY', 'YOUR_API_KEY')
QUERY      = 'データサイエンス 入門'
MAX_VIDEOS = 20
OUTPUT_CSV = 'youtube_stats.csv'

# ---- クライアント ----
youtube = build('youtube', 'v3', developerKey=API_KEY)

# ---- (b) 動画検索 ----
def search_videos(query, max_results=10):
    req = youtube.search().list(
        part='snippet', q=query, type='video',
        maxResults=max_results, relevanceLanguage='ja'
    )
    res = req.execute()
    return [
        {'video_id': item['id']['videoId'],
         'title'   : item['snippet']['title'],
         'published_at': item['snippet']['publishedAt']}
        for item in res.get('items', [])
    ]

# ---- (c) 動画統計 ----
def get_video_stats(video_ids):
    rows = []
    for i in range(0, len(video_ids), 50):
        chunk = video_ids[i:i+50]
        res = youtube.videos().list(
            part='snippet,statistics', id=','.join(chunk)
        ).execute()
        for item in res.get('items', []):
            s = item.get('statistics', {})
            rows.append({
                'video_id'     : item['id'],
                'title'        : item['snippet'].get('title', ''),
                'published_at' : item['snippet'].get('publishedAt', ''),
                'view_count'   : int(s.get('viewCount', 0)),
                'like_count'   : int(s.get('likeCount', 0)),
                'comment_count': int(s.get('commentCount', 0)),
            })
    return rows

# ---- 実行 ----
print(f'「{QUERY}」で動画を検索中...')
search_results = search_videos(QUERY, max_results=MAX_VIDEOS)
video_ids = [r['video_id'] for r in search_results]

print(f'{len(video_ids)} 件の動画の統計を取得中...')
stats_rows = get_video_stats(video_ids)

df = pd.DataFrame(stats_rows)
df = df.sort_values('view_count', ascending=False).reset_index(drop=True)
df.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
print(f'{OUTPUT_CSV} に保存しました.')
print(df[['title', 'view_count', 'like_count', 'comment_count']].to_string())
~~~

このコードで取得したデータは [Ch15 自然言語処理](slds15.html) のワードクラウド・トピックモデルの手法を `title` 列に適用したり, [Ch11 線形回帰分析](slds11.html) で `view_count` を目的変数とした回帰モデルを構築したりするときの入力として使えます.

# Quota・制限と X との比較

YouTube Data API と補足B で扱った X (Twitter) API を比較すると, 研究・学習での利用しやすさに大きな差があります.

| 項目 | YouTube Data API v3 | X (Twitter) API v2 |
|---|---|---|
| 無料枠 | **10,000 ユニット/日** (継続更新) | Basic プランは月 200 USD〜, 無料版は月 100 件のみ |
| 認証方式 | API キー (公開データ) / OAuth (個人データ) | Bearer Token (取得に英文 250 字の目的申請が必要) |
| 主なコスト | `search` = 100 U, `videos.list` = 1 U, `channels.list` = 1 U | リクエスト数・月次ツイート取得数で課金 |
| 過去データ | 投稿日で絞り込み可能 | 無料版は過去 7 日分のみ |
| 利用規約の安定性 | Google の方針変更は少ない | 頻繁に改定・値上げが行われている (2023〜) |

::: note
**Quota の消費例**: 動画 20 件の統計を取得するために `search().list()` を 1 回 (100 U) + `videos().list()` を 1 回 (1 U) = **101 ユニット** で済みます. 無料枠 10,000 U 内では約 99 回の同規模操作が可能です.

Quota が不足した場合は翌日 (太平洋時間の午前 0 時) にリセットされます. 使い切ってしまった場合は Google Cloud Console で追加 Quota を購入することもできますが, 学習目的では不要です.
:::

# `requests` を使った実装 (補足B との比較)

補足B では `requests` ライブラリを直接使って X API を叩きました. YouTube Data API でも同様に `requests` だけで実装できます. ライブラリ選択の方針については [補足A](slds_a1.html) の考え方が参考になります.

~~~ py
import requests
import pandas as pd

API_KEY = 'YOUR_API_KEY'
BASE_URL = 'https://www.googleapis.com/youtube/v3'

def search_videos_requests(query: str, max_results: int = 10) -> list[dict]:
    """requests を使ったキーワード検索"""
    url = f'{BASE_URL}/search'
    params = {
        'part'            : 'snippet',
        'q'               : query,
        'type'            : 'video',
        'maxResults'      : max_results,
        'relevanceLanguage': 'ja',
        'key'             : API_KEY,
    }
    response = requests.get(url, params=params)
    if response.status_code != 200:
        raise Exception(f'Error {response.status_code}: {response.text}')
    data = response.json()
    return [
        {'video_id': item['id']['videoId'],
         'title'   : item['snippet']['title']}
        for item in data.get('items', [])
    ]

# 動作確認
results = search_videos_requests('データサイエンス 入門', max_results=5)
print(pd.DataFrame(results))
~~~

`google-api-python-client` 版と比べると URL や パラメータ名を自前で管理する必要がありますが, `requests` だけで完結するため環境によっては導入が簡単です.

---

### Exercise SLDSc-1

**YouTube Data API による動画統計の取得**

本資料の worked example を参考にして, **任意のキーワード** (自分の研究テーマや興味のある分野) で YouTube 動画を検索し, 上位 10 件の再生数・高評価数・コメント数を取得して CSV ファイルに保存するプログラムを作成してください.

取得した DataFrame を使って, 以下の問いにも答えてください.

1. 再生数が最も多い動画のタイトルと再生数は何か?
2. 高評価数と再生数の間に相関はあるか? (`df.corr()` を使って確認してください.)

提出ファイル名: `sldsc-1.py`

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ py
import os
from googleapiclient.discovery import build
import pandas as pd

API_KEY = os.environ.get('YOUTUBE_API_KEY', 'YOUR_API_KEY')
QUERY   = 'データサイエンス 入門'   # ← 任意のキーワードに変更
youtube = build('youtube', 'v3', developerKey=API_KEY)

# 検索
search_res = youtube.search().list(
    part='snippet', q=QUERY, type='video',
    maxResults=10, relevanceLanguage='ja'
).execute()
video_ids = [item['id']['videoId'] for item in search_res.get('items', [])]

# 統計取得
stats_res = youtube.videos().list(
    part='snippet,statistics', id=','.join(video_ids)
).execute()

rows = []
for item in stats_res.get('items', []):
    s = item.get('statistics', {})
    rows.append({
        'video_id'     : item['id'],
        'title'        : item['snippet']['title'],
        'view_count'   : int(s.get('viewCount', 0)),
        'like_count'   : int(s.get('likeCount', 0)),
        'comment_count': int(s.get('commentCount', 0)),
    })

df = pd.DataFrame(rows).sort_values('view_count', ascending=False).reset_index(drop=True)
df.to_csv('sldsc-1.csv', index=False, encoding='utf-8-sig')
print(df[['title', 'view_count', 'like_count']])

# 問2: 相関
print('\n--- 相関係数 ---')
print(df[['view_count', 'like_count', 'comment_count']].corr())
~~~

**問 1**: `df.iloc[0]` で最大再生数の行を取り出せます.

**問 2**: 一般に `view_count` と `like_count` には正の相関が見られますが, コンテンツのジャンルや検索キーワードによって異なります. 相関が低い場合は「炎上系」や「BGM 系」動画が混在している可能性があります.

</details>

---

### Exercise SLDSc-2

**チャンネル比較分析**

2 つ以上の YouTube チャンネル (例: 大学の公式チャンネルや研究機関のチャンネル) のチャンネル ID を調べ, 本資料の `get_channel_stats()` 関数を使って登録者数・総再生数・動画本数を取得して比較する DataFrame を作成してください.

提出ファイル名: `sldsc-2.py`

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ py
import os
from googleapiclient.discovery import build
import pandas as pd

API_KEY = os.environ.get('YOUTUBE_API_KEY', 'YOUR_API_KEY')
youtube = build('youtube', 'v3', developerKey=API_KEY)

# 比較したいチャンネルの ID リスト (実際の ID に置き換えてください)
channel_ids = [
    'UCXXXXXXXXXXXXXXXXXXXXXXX',   # チャンネル1
    'UCYYYYYYYYYYYYYYYYYYYYYYY',   # チャンネル2
]

def get_channel_stats(channel_id):
    res = youtube.channels().list(
        part='snippet,statistics', id=channel_id
    ).execute()
    if not res.get('items'):
        return None
    item = res['items'][0]
    s = item['statistics']
    return {
        'channel_id'      : channel_id,
        'channel_name'    : item['snippet']['title'],
        'subscriber_count': int(s.get('subscriberCount', 0)),
        'view_count'      : int(s.get('viewCount', 0)),
        'video_count'     : int(s.get('videoCount', 0)),
    }

rows = [get_channel_stats(cid) for cid in channel_ids]
df = pd.DataFrame([r for r in rows if r is not None])
df = df.sort_values('subscriber_count', ascending=False).reset_index(drop=True)
df.to_csv('sldsc-2.csv', index=False, encoding='utf-8-sig')
print(df)
~~~

登録者数や総再生数の大小と, 動画本数の関係を観察してみましょう. 「少ない動画本数で高い総再生数を持つ」チャンネルはバズ動画を持つ可能性があります.

</details>
