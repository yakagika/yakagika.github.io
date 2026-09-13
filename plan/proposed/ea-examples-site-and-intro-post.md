---
plan_id: ea-examples-site-and-intro-post
status: proposed
created: 2026-09-13
updated: 2026-09-13
priority: medium
next_actor: user
next_action: "EA 側 Phase 0 の 4 点 (静的サイト生成器 / 公開元 branch / 図の扱い / Haddock 同居) を裁定する. 裁定は EA repo で handoff `blog-2026-09-13-ea-examples-site` を import した plan 上で行う. ブログ側 (本 plan の Phase 4) はサイト公開まで待機 (外部待ち b)"
---

# ExchangeAlgebra examples 解説サイト (英語) とブログの紹介記事

## メタ情報

- **状態**: proposed
- **作成日**: 2026-09-13
- **最終更新**: 2026-09-13
- **関係 repo**: 本体の作業は `haskell-exchange-algebra` (`~/Developer/Haskell/ExchangeAlgebra`) で行う.
  本 repo が持つのはブログの紹介記事と既存 EA 記事からのリンクだけ.
- **EA への依頼**: cross-repo handoff `blog-2026-09-13-ea-examples-site` (class: substantive).
  EA 側で `plans/proposed/` に import し, ユーザ承認を経て着手する.

## 概要

ExchangeAlgebra の examples を英語で解説するサイトを, EA の repo から GitHub Pages
(`https://yakagika.github.io/ExchangeAlgebra/`) として公開する. ブログには研究カテゴリの紹介記事を 1 本書き,
Research 一覧から辿れるようにする.

## 動機

- examples は README の catalogue と各 `.hs` のコメントしか説明が無く, 海外の利用者が
  「何を表す例か」「どう読むか」を追えない.
- ブログは日本語で, 講義資料とナビゲーションを共有している. EA の英語解説を混ぜると
  言語もビルド系も合わない. ブログとは独立したサイトにする (2026-09-13 ユーザ決定).
- 解説と example のコードを同じ repo に置けば, API を変えたときに同じ変更で解説も直せる.
  別 repo だとコードの複製か版の固定が必要で, ずれやすい (2026-09-13 ユーザ決定: EA repo から公開).

## 設計

### URL と衝突確認

- ブログは user site (`yakagika/yakagika.github.io`) なので, EA repo の Pages は
  `https://yakagika.github.io/ExchangeAlgebra/` になる.
- ブログの出力 `docs/` の top level に `ExchangeAlgebra/` は無い (2026-09-13 確認). 衝突しない.
  ブログ側に同名ディレクトリを作らないこと.

### サイトの対象範囲 (EA の既存計画との整合)

EA の `plans/in-progress/example-ownership.md` §2 の所有区分に合わせる.

| 区分 | 対象 (2026-09-04 時点) | サイトでの扱い |
|---|---|---|
| in-tree (教育) | `ebex1`-`ebex9` (basic), `cge` (optimization/CGE) | 解説ページを作る |
| 二重用途 | `sim1` / `sim2` (basic), `industrialEx1`, `invoiceEx1`, `cge-lite` | 解説ページを作る (EA が正本) |
| 論文 repo 所有 | `ripple` 系, `market` 系 | 解説しない. 0.6.0.0 で論文 repo へ移る予定なので, 論文への pointer だけ置く |

- 各 family の目的・手法・論文参照は `examples-metadata-8a` で整備された `examples/<dir>/family.yaml`
  を入力にし, 索引ページの記述を二重管理しない.
- 解説中のコードは example のファイルから**抜粋を取り込む** (手で貼らない). 取り込み方は生成器に依存する (Phase 0).

### 公開方式

- GitHub Actions でビルドし, Pages へ deploy する. 生成 HTML は commit しない
  (EA の branch 運用に生成物の差分を持ち込まない).
- CI で example を build してからサイトを build し, 解説が参照するコードが壊れていれば公開しない.

### ブログ側

- `posts/<公開日>-exchangealgebra-examples-site.md` を `category: research` で書く (日本語, WRITING_STYLE.md 準拠).
  内容: サイトの目的, 収録している例の範囲, 読む順番, リンク.
- 既存の `posts/2026-05-26-exchangealgebra-on-hackage.md` と `posts/2026-09-04-exchangealgebra-0-5-0-0.md` から
  紹介記事かサイトへリンクを足す.
- `templates/research.html` は変更しない (紹介記事方式を採用, 2026-09-13 ユーザ決定).

## ロードマップ

| Phase | repo | 内容 | 完了条件 |
|---|---|---|---|
| 0 | EA | 未決 4 点の裁定 (下記) | EA plan に裁定を記録 |
| 1 | EA | サイトの骨組み + Actions deploy + repo 設定で Pages 有効化 (外向き操作なのでユーザ確認) | 空の index が URL で見える |
| 2 | EA | basic family (簿記 `ebex*`, `sim1`/`sim2`) の解説 | CI で example build → site build → deploy が通る |
| 3 | EA | industrial / invoice / CGE / cge-lite の解説, 論文所有 example への pointer | 教育・二重用途の全 family に解説がある |
| 4 | blog | 紹介記事の執筆, 既存 EA 記事からのリンク, `stack exec site build` で確認 | Research 一覧に記事が出て, リンク先が開く |

Phase 4 はサイト公開 (Phase 2 完了) 以降に着手する. 記事は公開済みの範囲だけを書く.

## コスト見積もり

- Phase 0-1: 1 セッション.
- Phase 2-3: family ごとに 1 セッション前後 (5 family 程度). 英文の下書きは codex に出し, 裁定・統合を Claude が行う.
- Phase 4: 1 セッション.

## 未解決事項 / リスク

- **(Phase 0) 静的サイト生成器**: mdBook (`{{#include file:anchor}}` でコード抜粋を取り込める, Haskell の build 系と独立) /
  MkDocs / Hakyll (ブログと同系統だが, EA の stack project に site 用 package が増える). ユーザ判断 (a).
- **(Phase 0) 公開元 branch**: master (リリース線, Hackage 版と一致) か develop (統合線, 最新 API).
  `examples/stack.yaml` が Hackage の 0.5.0.0 を pin しているので, 利用者が読む版と揃うのは master. ユーザ判断 (a).
- **(Phase 0) 図の扱い**: example の図は uv + Python で生成し, 出力は `.gitignore` 済み. CI で生成する
  (GHC + uv が要り重い) か, 解説用の図だけ別に commit するか. ユーザ判断 (a).
- **(Phase 0) Haddock の同居**: README は Haddock を htmlpreview 経由で示している. 同じ Pages の
  `/ExchangeAlgebra/haddock/` に置くか, 本計画の範囲外にするか. ユーザ判断 (a).
- **0.6.0.0 の example 移動**: `ripple` / `market` が論文 repo へ移ると pointer の張り替えが要る.
  サイトの索引を `family.yaml` から生成しておけば追随は機械的になる.
- **Pages の有効化は外向き操作**: repo 設定の変更と初回公開はユーザ確認のうえで行う (c).

## 関連ファイル

- ブログ: `src/Main.hs` (`isResearch`, `research.html` の生成), `templates/research.html`,
  `posts/2026-05-26-exchangealgebra-on-hackage.md`, `posts/2026-09-04-exchangealgebra-0-5-0-0.md`
- EA: `examples/README.md`, `examples/*/family.yaml`, `examples/scripts/gen_catalogue.py`,
  `.github/workflows/ci.yml`, `plans/in-progress/example-ownership.md`, `plans/in-progress/examples-metadata-8a.md`
- handoff: `~/Developer/claude/assistant/state/cross-repo-handoffs/haskell-exchange-algebra/queue/blog-2026-09-13-ea-examples-site.md`

## 変更履歴

- 2026-09-13: 作成 (EA repo から公開 + 紹介記事方式をユーザが決定)
