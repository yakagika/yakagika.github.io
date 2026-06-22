# EDINET 財務データ分析パイプライン

特別講義DS 補足A「EDINET API による財務データの取得と回帰分析」の配布コード.
元になったのは受講生 (ピエレット・フィリップ氏) の研究コードで, 講義用に
リファクタリングしたもの. 詳細な解説は講義資料の補足Aを参照.

## 実行順

```
uv add requests pandas yfinance statsmodels seaborn matplotlib-fontja
export EDINET_API_KEY=<各自取得したキー>

uv run python fetch_documents.py        # 1. 書類一覧 + CSV zip の取得
uv run python build_financial_table.py  # 2. zip -> 企業 x 年度の財務表
uv run python compute_indicators.py     # 3. 財務指標の計算
uv run python fetch_prices.py           # 4. 株価の取得 (証券コード経由)
uv run python merge_all.py              # 5. 業種・株価・(任意) ESG の結合
uv run python regression.py             # 6. 可視化と重回帰分析
```

## 必要な外部データ

- `data/jpx_industry.csv` — JPX (日本取引所グループ) が公表する東証上場銘柄一覧
  から「コード」「33業種区分」の 2 列を取り出した CSV. 銘柄一覧は
  https://www.jpx.co.jp/markets/statistics-equities/misc/01.html から取得できる.
- `data/esg_scores.csv` (任意) — `secCode`, `年度`, `ESGスコア` の 3 列.
  実 ESG スコアは商用データ (FTSE Russell 等) のため同梱していない.
  動作確認用の**乱数による合成スコア**を `slds_data/a1/esg_scores_synthetic.csv`
  として配布している (乱数なので分析結果に実質的な意味はない. 詳細は補足A).

## 注意

- API キーは EDINET で各自取得し, 環境変数 `EDINET_API_KEY` で渡す.
  キーをコードに書いた場合, そのファイルを共有・提出しないこと.
- EDINET / Yahoo Finance とも利用規約とサーバ負荷に配慮すること
  (取得は必要な期間に絞る, リクエスト間に sleep を入れる).
