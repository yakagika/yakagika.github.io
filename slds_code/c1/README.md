# YouTube コメントのネガポジ分析 (市川市データセット)

特別講義DS 補足C「YouTube Data API による動画・チャンネルデータの取得」の配布コード.
詳しい解説は講義資料の補足Cを参照.

## 構成

- `build_dataset.py` — 市川市に関する動画を数件取得し, タイトル・ID・統計・コメントを
  1 つの CSV (`ichikawa_youtube.csv`, 1 行 = 1 コメント) にまとめる. **API キーが必要**.
- `sentiment_analysis.py` — `ichikawa_youtube.csv` を読み込み, 各コメントを日本語 BERT で
  ネガポジ判定し, 感情比率 (円グラフ) とポジ率×高評価数 (散布図) を出力する. **API キー不要**.

## 実行順

```
# 1. データセットの作成 (API キーが必要)
uv add google-api-python-client pandas
export YOUTUBE_API_KEY=<各自取得したキー>
python build_dataset.py            # → ichikawa_youtube.csv

# 2. 感情分析 (CSV だけで動く)
uv add pandas matplotlib matplotlib-fontja transformers fugashi unidic-lite torch tqdm
python sentiment_analysis.py       # → sentiment-pie.png, sentiment-scatter.png
```

`ichikawa_youtube.csv` は配布もしているので, API キーが無い場合は CSV をダウンロードして
手順 2 から始められます.

## CSV の列

| 列 | 内容 |
|---|---|
| `video_id` | 動画 ID |
| `title` | 動画タイトル |
| `view_count` / `like_count` / `comment_count` | 動画の統計 (各コメント行に繰り返し) |
| `comment` | コメント本文 |
| `comment_like_count` | コメントのいいね数 |
| `comment_published_at` | コメントの投稿日時 |
