# X (Twitter) API による投稿データの取得

特別講義DS 補足B「X(Twitter) API によるデータの取得」の配布コード.
詳しい解説は講義資料の補足Bを参照.

## 構成

- `fetch_posts.py` — 検索ワードごとに X API v2 の recent search (直近 7 日) で投稿を取得し,
  1 行 = 1 投稿の CSV (`posts.csv`) にまとめる. **認証トークンが必要** (`--dry-run` を除く).

出力 CSV の列は補足C の `ichikawa_youtube.csv` と揃えてあるので, 補足C の
`sentiment_analysis.py` は `INPUT_CSV` を `posts.csv` に書き換えるだけで流用できる.

## 実行順

```
uv add requests pandas

# 1. 課金される前に取得件数と概算費用だけ確認する (トークン不要)
python fetch_posts.py --dry-run

# 2. 実際に取得する (実行前に確認プロンプトが出る)
export X_BEARER_TOKEN=<教員から配布されたトークン>
python fetch_posts.py              # → posts.csv

# 3. 感情分析 (補足C のコードを流用. INPUT_CSV = 'posts.csv' に変更する)
python ../c1/sentiment_analysis.py
```

## 費用について

X API は 2026-02 の改定で従量課金のみとなり, 投稿の取得は **1 件 0.005 USD** です.
`QUERIES` の語数 × `MAX_RESULTS` がそのまま取得件数 = 費用になります.
`MAX_RESULTS` は `HARD_CAP` (既定 100) で頭打ちになり, それ以上は取得しません.

前払いしたクレジット残高がそのまま支出の上限になります. 配布トークンは共有の残高に
直結しているので, 件数を増やす前に必ず `--dry-run` で確認してください.

## CSV の列

| 列 | 内容 |
|---|---|
| `video_id` | 検索ワード (集計の単位. 補足C の動画 ID に相当) |
| `title` | 検索ワード |
| `view_count` | そのワードで取得した投稿の表示回数の合計 |
| `like_count` | そのワードで取得した投稿のいいね数の合計 |
| `comment_count` | そのワードで取得した投稿数 |
| `comment` | 投稿本文 |
| `comment_like_count` | 投稿のいいね数 |
| `comment_published_at` | 投稿日時 |
| `post_id` / `author_id` | 投稿 ID / 投稿者 ID |
| `retweet_count` / `reply_count` / `impression_count` | 投稿ごとの指標 |

前 8 列が補足C と共通で, 残りは X 固有の追加列です. `view_count` / `like_count` /
`comment_count` はワード単位で集計した値なので, 同じワードの行にはすべて同じ値が入ります
(補足C で動画統計が各コメント行に繰り返されるのと同じ形).
