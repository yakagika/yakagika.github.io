---
plan_id: slds-edinet-supplement
status: in-progress
created: 2026-06-12
updated: 2026-06-12
next_action: ドラフト一式のユーザレビュー (補足資料 slds_a1.md / slds_code/edinet/ 6 本 / Ch6・Ch11・Ch15 リンク). 公開は publish.sh (ユーザ実行). 元 API キーの失効は学生へ要連絡
next_actor: user
---

# slds 補足資料: EDINET API による財務データの取得と回帰分析

## メタ情報

- **状態**: in-progress
- **作成日**: 2026-06-12
- **最終更新**: 2026-06-12
- **発端**: 学生 (ピエレット・フィリップ氏) が研究で作成した EDINET データ取得・
  整形・回帰分析の一連のコード (tmp/Philippe/, 11 スクリプト) を整理し,
  他の学生が利用可能な資産として slds 講義に組み込む (ユーザ依頼 2026-06-12).
- **配置案の決定**: AskUserQuestion で「(a)+(b) 補足資料 + リンク」を採用.
  章番号外の補足資料を新設し, Ch6 / Ch11 / Ch15 から相互リンク.
  フィリップ氏の論文 (slds_papers/2026_ピエレット.pdf, FTSE Russell ESG スコア
  × EDINET 財務データの重回帰分析) を成果例として紹介する.

## 概要

EDINET API v2 を使った有価証券報告書データの取得から, XBRL CSV の整形,
株価・業種・ESG スコアとの結合, 重回帰分析までを一通り解説する補足資料を
`lectures/slds/slds_a1.md` として新設する. 元コードはリファクタリングし
問題点を修正した版を掲載・配布する.

## 設計

### 成果物

1. **`lectures/slds/slds_a1.md`** — 補足資料本体 (→ `lectures/slds_a1.html`).
   章番号体系の外 (タイトル「特別講義DS 補足A ...」). previousChapter/nextChapter
   は設定しない (各章からのリンクで到達).
2. **`slds_code/edinet/`** — 改善版スクリプト一式 (資料からダウンロード可能に).
   元の 11 本を 5-6 本に再構成.
3. **既存章へのリンク追加** (講義資料編集規約: 前後の記述順序・未説明用語の
   confirmation 必須):
   - `slds6.md` — EDINET 言及箇所 (l.209 付近) から補足へのリンク.
   - `slds11.md` — 線形回帰の実データ事例として補足 + 論文を紹介.
   - `slds15.md` — API 一般説明の節に「EDINET の事例は補足A」リンク.

### 元コードの主要な問題点と改善方針

| 問題 | 改善 |
|---|---|
| API キーがソースに直書き (実キー露出) | 空欄の定数 + 環境変数. **元キーは失効推奨 (要ユーザ→学生連絡)** |
| Windows 絶対パスのハードコード | pathlib + スクリプト相対パス |
| 企業名文字列でのマージ (正規化関数が 3 実装) | 取得時に docID/証券コード/EDINET コードの索引 CSV を作り, 以後の結合は証券コードで行う |
| 純資産 fallback の二重計上 (利益剰余金+繰越利益剰余金 等) | coalesce 方式 (純資産系列の優先順位フォールバックのみ) |
| 経常利益 fallback 式の誤り (営業利益欠落) | 正しい定義式 + 欠損は欠損のまま |
| `replace("-", 0)` / `dropna(how="any")` | 欠損は NaN 維持, 分析直前に変数単位で dropna |
| 3/31 固定の株価 (休日欠損→企業脱落) | 期末日以前の直近営業日 (asof) |
| Yahoo 検索による企業名→ティッカー解決 | 証券コード + ".T" で決定的に解決 |
| PBR の `round` 位置バグ | 修正 |
| ステップワイズ後の p 値による再選択 | 統計的問題 (多重検定) を本文で解説し, 全変数モデル + AIC 比較の形に |
| bare except / print デバッグ / global | 例外の局所化, logging 風の最小出力 |

### データの扱い (AskUserQuestion で決定: 出所確認後に同梱検討)

- **ESG スコア**: 論文より FTSE Russell の総合スコアと判明. **商用データのため
  同梱不可**. 資料では出所を明記し「各自が利用可能なスコアで代替」とする.
- **JPX 業種区分**: 東証公表の銘柄一覧 (公開資料). ダウンロードリンクを記載し
  各自取得.
- **EDINET データ**: 各自が API キーを取得して各自ダウンロード (API キー取得
  手順は EDINET 公式資料に準拠して記載, キー欄は空欄).

## ロードマップ

1. EDINET API v2 の公式仕様 (キー取得手順, エンドポイント) を確認 ✅→作業中
2. `slds_code/edinet/` に改善版スクリプトを作成
3. `lectures/slds/slds_a1.md` を執筆
4. Ch6 / Ch11 / Ch15 にリンク追加 (前後矛盾チェック)
5. `stack exec main build` で検証, ユーザレビュー

## 変更履歴

- **2026-06-12 (補足シリーズ化)**: ユーザ決定により補足を「APIによるデータ取得」
  シリーズとして再構成. 補足A = EDINET (API 一般論 REST/HTTP/JSON を Ch15 から移設,
  正本化), 補足B = X(Twitter) (Ch15 の取得節を移設・新設 `slds_b1.md`. 全体コードの
  未定義変数 `party` バグを修正, pip → uv 化). Ch15 の Twitter 節はリンクと
  tweets.csv の明示のみに縮約. ナビ: Ch17 → 補足A → 補足B. **YouTube API の補足C を
  今後追加予定** (ユーザ予告 2026-06-12). 配置規約は CLAUDE.md に記録済.
- **2026-06-12 (数式修正)**: 指標定義の数式が壊れていた原因は KaTeX 本体ではなく,
  サイトの auto-render が `\\[4pt]` 中の `\[` を数式区切りと誤認すること (KaTeX
  0.16.0 で当該数式が renders することは node で実証). 行送りは `\\` のみ使用する
  こと (サイト既存数式と同じ規約).

## 検証結果 (2026-06-12, 実 API キーによる end-to-end テスト)

ユーザの API キー (~/.edinet_api_key 経由, repo 外) で全 6 スクリプトを /tmp 隔離環境で実行:

- fetch_documents (2025-06-10〜13, 有報 39 件): OK. 一覧 API が同一 docID を重複して
  返すケースを 1 件検出 → drop_duplicates(subset="docID") を追加修正.
- build_financial_table: 34 社 168 行 (企業 × 年度) 生成 OK.
- compute_indicators: 実在企業の公表値と整合 (ZOZO ROA 25-30%, カワチ薬品 自己資本比率 52-57%).
- fetch_prices: 32/34 社 OK (1 社は名証単独上場で Yahoo 未収載 — 想定内の欠損).
- merge_all: JPX data_j.xls を実取得し README の手順どおり jpx_industry.csv 化 → 業種結合 158/168 行 (94%).
  PBR も実勢と整合 (ZOZO 12.9 / カワチ薬品 0.55).
- regression: 合成 ESG (乱数, テスト専用・非コミット) で実行 OK. 乱数 ESG の係数 ≈ 0
  (p=0.977), AIC は統制のみ < +ESG — 無関係な変数を正しく無関係と判定.

## 未解決事項 / リスク

- 元 API キーの失効はユーザから学生へ依頼が必要.
- 演習 (Exercise SLDSA1-k 形式) を付けるかは未定 (初版では付けない).
- 元コードの ESG CSV (esg20XX_2cols.csv) の生成過程は資料化対象外
  (FTSE Russell データの加工は同梱不可のため).
