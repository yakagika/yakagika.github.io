# X (Twitter) API による投稿データの取得

特別講義DS 補足B「X(Twitter) API によるデータの取得」の配布コード.
詳しい解説は講義資料の補足Bを参照.

## 構成

- `fetch_posts.py` — 検索ワードごとに X API v2 の recent search (直近 7 日) で投稿を取得し,
  1 行 = 1 投稿の CSV (`posts.csv`) にまとめる. **認証トークンが必要** (`--dry-run` を除く).
- `to_youtube_format.py` — `posts.csv` を補足C の `ichikawa_youtube.csv` と同じ列構成
  (`posts_youtube_format.csv`) に変換する. 補足C の `sentiment_analysis.py` を流用するとき
  だけ使う. **API キー不要**.

## 実行順

```
uv add requests pandas

# 1. 課金される前に取得件数と概算費用だけ確認する (トークン不要)
python fetch_posts.py --dry-run

# 2. 実際に取得する (実行前に確認プロンプトが出る)
export X_BEARER_TOKEN=<教員から配布されたトークン>
python fetch_posts.py              # → posts.csv

# 3. (補足C の感情分析を流用する場合) 列構成を変換する
python to_youtube_format.py        # → posts_youtube_format.csv

# 4. 感情分析 (補足C のコードで INPUT_CSV = 'posts_youtube_format.csv' に変更する)
python ../c1/sentiment_analysis.py
```

## 費用について

X API は 2026-02 の改定で従量課金のみとなり, 投稿の取得は **1 件 0.005 USD** です.
`QUERIES` の語数 × `MAX_RESULTS` がそのまま取得件数 = 費用になります.
`MAX_RESULTS` は `HARD_CAP` (既定 100) で頭打ちになり, それ以上は取得しません.

前払いしたクレジット残高がそのまま支出の上限になります. 配布トークンは共有の残高に
直結しているので, 件数を増やす前に必ず `--dry-run` で確認してください.

## posts.csv の列 (1 行 = 1 投稿)

| 列 | 内容 |
|---|---|
| `query` | 検索ワード |
| `text` | 投稿本文 |
| `like_count` / `retweet_count` / `reply_count` | いいね数 / リツイート数 / 返信数 |
| `impression_count` | 表示回数 (取れない投稿では 0) |
| `created_at` | 投稿日時 |
| `post_id` / `author_id` | 投稿 ID / 投稿者 ID |

Ch15 (自然言語処理) の配布データ `tweets.csv` もこの構造 (`query`, `text` の 2 列) で,
Ch15 のコードは `posts.csv` をそのまま読める.

## posts_youtube_format.csv への変換規則

集計の単位を「動画 1 本」から「検索ワード 1 語」に対応させる. 詳細な対応表は講義資料
補足B の「補足C形式への変換」節を参照.

| 補足C の列 | 変換規則 |
|---|---|
| `video_id` / `title` | `query` をそのまま |
| `view_count` | `impression_count` のワード単位合計 |
| `like_count` | `like_count` のワード単位合計 |
| `comment_count` | ワード単位の行数 |
| `comment` | `text` |
| `comment_like_count` | `like_count` |
| `comment_published_at` | `created_at` |

ワード単位の集計列は同じワードの行にすべて同じ値が入る (補足C で動画統計が各コメント行に
繰り返されるのと同じ形).
